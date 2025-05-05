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

(define (check-query? exp)
  (tagged-list? exp 'check))


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


(define (list-recipes expression environment)
  (let ((recipes (get-recipes environment)))
    (let scan ((r recipes))
      (if (pair? r)
	  (begin
	    (write-line (recipe-name (car r)))
	    (scan (cdr r)))))
    (list (length recipes) 'recipes)))


(define (load-recipes expression environment)
  (let ((filename (car expression)))
    (let ((lines (read-sexprs-from-file filename)))
      (let ((recipes (map (lambda (item)
			    (let ((name (car item))
				  (recipe-items (cadr item))
				  (provenance (caddr item)))
			      (make-recipe name
					   (map (lambda (row)
						  (let ((n (car row))
							(a (cadr row))
							(u (caddr row)))
					   	  (make-recipe-item n a u)))
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


(define restrictions-list
  (list (cons 'vegetarian vegetarian)))


(define (check-recipe expression environment)
  (let ((name (car expression))
	(restr (cadr expression)))
    (let ((recipe (find-recipe-by-name name (get-recipes environment)))
	  (test (assv restr restrictions-list)))
      (pp (list 'test recipe test))
      (if (and recipe test)
	  (restr:check-recipe (cdr test) recipe)
	  'invalid-restriction))))


(define (eval-recipes-query expression environment)
  (cond ((list-query? expression)
	 (list-recipes (cdr expression) environment))
	((load-query? expression)
	 (load-recipes (cdr expression) environment))
	((check-query? expression)
	 (check-recipe (cdr expression) environment))
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
(recipes check "Crispy Baked Chicken Thighs" vegetarian)


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
