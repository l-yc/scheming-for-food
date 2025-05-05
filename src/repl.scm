;(load "./sdf/manager/load")
;(manage 'new 'generic-procedures)

(load "ingredient.scm")
(load "recipe.scm")
(load "data_loader.scm")
(load "restrictions.scm")


;; REPL

;; from SDF
(define (environment? x) #t)     ;make better!
(register-predicate! environment? 'environment)

(define (extend-environment variables values base-environment)
  (if (not (fix:= (length variables) (length values)))
      (if (fix:< (length variables) (length values))
          (error "Too many arguments supplied" variables values)
          (error "Too few arguments supplied" variables values)))
  (vector variables values base-environment))

(define (environment-variables env) (vector-ref env 0))
(define (environment-values env) (vector-ref env 1))
(define (environment-parent env) (vector-ref env 2))

(define the-empty-environment (list '*the-empty-environment*))

(define (lookup-variable-value var env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
        (lookup-scheme-value var)
        (let scan
            ((vars (vector-ref env 0))
             (vals (vector-ref env 1)))
          (cond ((null? vars) (plp (vector-ref env 2)))
                ((eq? var (car vars)) (car vals))
                (else (scan (cdr vars) (cdr vals))))))))

(define lookup-scheme-value
  (let ((env (the-environment)))
    (named-lambda (lookup-scheme-value var)
      (lexical-reference env var))))

(define (define-variable! var val env)
  (let scan
      ((vars (vector-ref env 0))
       (vals (vector-ref env 1)))
    (cond ((null? vars)
           (vector-set! env 0 (cons var (vector-ref env 0)))
           (vector-set! env 1 (cons val (vector-ref env 1))))
          ((eq? var (car vars))
           (set-car! vals val))
          (else
           (scan (cdr vars) (cdr vals))))))

(define (set-variable-value! var val env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var))
    (let scan
        ((vars (vector-ref env 0))
         (vals (vector-ref env 1)))
      (cond ((null? vars) (plp (vector-ref env 2)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))))

(define initial-env-bindings '())

(define (define-initial-env-binding name value)
  (let ((p (assq name initial-env-bindings)))
    (if p
        (set-cdr! p value)
        (set! initial-env-bindings
              (cons (cons name value) initial-env-bindings))))
  name)

(define (make-global-environment)
  (extend-environment (map car initial-env-bindings)
                      (map cdr initial-env-bindings)
                      the-empty-environment))

(define the-global-environment
  'not-initialized)

(define (initialize-repl!)
  (define-initial-env-binding '%recipes '())
  (define-initial-env-binding '%ingredients '())
  (set! the-global-environment (make-global-environment))
  'done)

(define (check-repl-initialized)
  (if (eq? the-global-environment 'not-initialized)
      (error "Interpreter not initialized. Run (init) first.")))

(define (g:read)
  (prompt-for-command-expression "sff> "))

(define (init)
  (initialize-repl!)
  (repl))

(define (go)
  (repl))

(define (repl)
  (check-repl-initialized)
  (let ((input (g:read)))
    (write-line (q:eval input the-global-environment))
    (repl)))








;; handlers

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (ingredients-query? exp)
  (tagged-list? exp 'ingredients))

(define (recipes-query? exp)
  (tagged-list? exp 'recipes))

(define (list-query? exp)
  (tagged-list? exp 'list))

(define (load-query? exp)
  (tagged-list? exp 'load))

(define (search-query? exp)
  (tagged-list? exp 'search))

(define (show-query? exp)
  (tagged-list? exp 'show))

(define (check-query? exp)
  (tagged-list? exp 'check))

(define (scale-query? exp)
  (tagged-list? exp 'scale))

(define (filter-query? exp)
  (tagged-list? exp 'filter))


;; helpers
(define (read-to-list-by-line filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '())
                 (next-line (read-line)))
	(if (eof-object? next-line)
	    (reverse lines)         
	    (let ((thing (read (open-input-string next-line))))
	      (loop (cons thing lines)
		    (read-line))))))))


(define (read-all-sexprs port)
  (let loop ((acc '()))
    (let ((x (read port)))
      (if (eof-object? x)
          (reverse acc)
          (loop (cons x acc))))))


(define (read-sexprs-from-file filename)
  (call-with-input-file filename
    (lambda (port)
      (read-all-sexprs port))))


;; ingredients
(define (get-ingredients environment)
  (lookup-variable-value '%ingredients environment))


(define (list-ingredients expression environment)
  (let ((ingredients (get-ingredients environment)))
    (let scan ((i ingredients))
      (if (pair? i)
	  (begin
	    (write-line (ingredient-name (car i)))
	    (scan (cdr i)))))
    (list (length ingredients) 'ingredients)))

  
(define (load-ingredients expression environment)
  (let ((filename (car expression)))
    (let ((lines (read-sexprs-from-file filename)))
      (let ((ingredients (map (lambda (item)
				(let ((name (car item))
				      (tags (cdr item)))
				  (make-ingredient name tags))) lines)))
	(set-variable-value! '%ingredients
			     ingredients
			     environment)
	(list (length ingredients) 'ingredients)))))
  

(define (eval-ingredients-query expression environment)
  (cond ((list-query? expression)
	 (list-ingredients (cdr expression) environment))
	((load-query? expression)
	 (load-ingredients (cdr expression) environment))
	(else 'invalid-ingredients-query)))


(define (get-recipes environment)
  (lookup-variable-value '%recipes environment))


(define (display-recipes recipes)
  (let scan ((r recipes))
    (if (pair? r)
	(begin
	  (write-line (recipe-name (car r)))
	  (scan (cdr r)))))
  (list (length recipes) 'recipes))

  
(define (list-recipes expression environment)
  (display-recipes (get-recipes environment)))


(define (load-recipes expression environment)
  (let ((filename (car expression)))
    (let ((lines (read-sexprs-from-file filename)))
      (let ((recipes (map (lambda (item)
			    (let ((name (car item))
				  (recipe-items (cadr item))
				  (provenance (caddr item)))
			      (make-recipe name
					   (map (lambda (row)
						  (apply make-recipe-item row))
						  ;(let ((n (car row))
						  ;	(a (cadr row))
						  ;	(u (caddr row)))
					   	  ;(make-recipe-item n a u)))
					   	recipe-items)
					   provenance)))
			  lines)))
	(set-variable-value! '%recipes
			     recipes
			     environment)
	(list (length recipes) 'recipes)))))


(define (find-recipe-by-name name recipes)
  (let ((result (filter (lambda (recipe)
			  (string=? (recipe-name recipe) name))
			recipes)))
    (if (> (length result) 0)
	(car result)
	#f)))


(define (find-recipes-by-substring needle recipes)
  (filter (lambda (recipe)
	    (substring? (string-upcase needle)
			(string-upcase (recipe-name recipe))))
	  recipes))


(define restrictions-list
  (list
   (cons 'vegetarian vegetarian)
   (cons 'halal halal)
   (cons 'kosher kosher)))


(define (search-recipes expression environment)
  (let ((needle (car expression)))
    (let ((recipes (get-recipes environment)))
      (display-recipes (find-recipes-by-substring needle recipes)))))


(define (display-recipe recipe)
  (display "Recipe: ")
  (display (recipe-name recipe))
  (newline)
  (display "---")
  (newline)
  (let scan ((items (recipe-items recipe)))
    (if (pair? items)
	(begin
	  (display "* ")
	  (display (ingredient-name (recipe-item-ingredient (car items))))
	  (display ", ")
	  (display (quantity-amount (recipe-item-quantity (car items))))
	  (display " ")
	  (display (quantity-unit (recipe-item-quantity (car items))))
	  (newline)
	  (scan (cdr items)))))
  (list (length (recipe-items recipe)) 'items))

  
(define (show-recipe expression environment)
  (let ((name (car expression)))
    (let ((recipe (find-recipe-by-name name (get-recipes environment))))
      (display-recipe recipe))))


(define (check-recipe expression environment)
  (let ((name (car expression))
	(restr (cadr expression)))
    (let ((recipe (find-recipe-by-name name (get-recipes environment)))
	  (test (assv restr restrictions-list)))
      (if (and recipe test)
	  (restr:check-recipe (cdr test) recipe)
	  'invalid-restriction))))


(define (scale-recipe-item item scale)
  (let ((ing (recipe-item-ingredient item))
	(qty (recipe-item-quantity item))
	(rule (recipe-item-scaling-rule item)))
    (cond ((constant-scaling-rule? rule)
	   item)
	  ((linear-scaling-rule? rule)
	   (make-recipe-item (ingredient-name ing)
			     (* (quantity-amount qty)
				(linear-scaling-rule-factor rule)
				scale)
			     (quantity-unit qty)))
	  (else 'scaling-rule-not-implemented))))

  
(define (scale-recipe expression environment)
  (let ((name (car expression))
	(scale (cadr expression)))
    (let ((recipe (find-recipe-by-name name (get-recipes environment))))
      (let ((scaled-items
	     (map (lambda (item) (scale-recipe-item item scale))
		  (recipe-items recipe))))
	(display-recipe (make-recipe (recipe-name recipe)
				     scaled-items
				     (recipe-provenance recipe)))))))


(define (filter-recipes expression environment)
  (let ((restr (car expression)))
    (let ((recipes (get-recipes environment))
	  (test (assv restr restrictions-list)))
      (if test
	  (display-recipes
	   (filter (lambda (recipe) (restr:check-recipe (cdr test) recipe))
		   recipes))
	  (display-recipes
	   (filter (lambda (recipe) (restr:check-recipe restr recipe))
		   recipes))))))


(define (eval-recipes-query expression environment)
  (cond ((list-query? expression)
	 (list-recipes (cdr expression) environment))
	((load-query? expression)
	 (load-recipes (cdr expression) environment))
	((search-query? expression)
	 (search-recipes (cdr expression) environment))
	((show-query? expression)
	 (show-recipe (cdr expression) environment))
	((check-query? expression)
	 (check-recipe (cdr expression) environment))
	((scale-query? expression)
	 (scale-recipe (cdr expression) environment))
	((filter-query? expression)
	 (filter-recipes (cdr expression) environment))
	(else 'invalid-recipes-query)))

  
(define (q:eval expression environment)
  (cond ((ingredients-query? expression)
	 (eval-ingredients-query (cdr expression) environment))
	((recipes-query? expression)
	 (eval-recipes-query (cdr expression) environment))
	(else 'invalid-query)))

	 

(init)
(go)

(ingredients load "examples/ingredients.txt")
(ingredients list)
(recipes load "examples/recipes.txt")
(recipes list)
(recipes search "chicken")
(recipes check "Crispy Baked Chicken Thighs" halal)
(recipes show "Crispy Baked Chicken Thighs")
(recipes scale "Crispy Baked Chicken Thighs" 2) ; note olive oil unchanged!
(recipes filter vegetarian)
(recipes filter halal)
(recipes filter kosher)
(recipes filter (restr-all (recip-all (ing-not (ing-is poultry)))))


;; too complicated, i think i'll just bash this
;(define q:eval
;  (simple-generic-procedure 'q:eval 2 default-eval))
;
;(define (environment? x) #t)
;
;(environment? the-global-environment)
;
;(define (eval-ingredients-query expression environment)
;  
;  'ingredients)
;
;(define-generic-procedure-handler q:eval
;  (match-args ingredients-query? environment?)
;  eval-ingredients-query)
