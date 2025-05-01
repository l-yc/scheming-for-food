(load "utils.scm")

;; searching recipes
(define (find-recipes-with name)
  (filter (lambda (recipe)
	    (any
	     (lambda (recipe-item)
	       (string=? (ingredient-name (recipe-item-ingredient recipe-item))
			 name))
	     (recipe-items recipe)))
	  recipes))

;; tests
(find-recipes-with "chicken thighs")
(find-recipes-with "salt")

;; summarizing recipes
(define (add-recipe-items i1 i2)
  (let ((i (recipe-item-ingredient i1))
	(q1 (recipe-item-quantity i1))
	(q2 (recipe-item-quantity i2)))
    ;; FIXME  need to handle unit conversion?
    ;;(assert (eq? (quantity-unit q1) (quantity-unit q2)))
    (make-recipe-item (ingredient-name i)
		      (+ (quantity-amount q1) (quantity-amount q2))
		      (quantity-unit q2))))
  
(define (get-shopping-list-of recipes)
  (let* ((ingredients (filter
		       (lambda (ri) (if (recipe-item-ingredient ri) #t #f))
		       (append-map recipe-items recipes)))
  	 (sorted (sort ingredients
  		       (lambda (a b) (string<? (ingredient-name (recipe-item-ingredient a))
  					       (ingredient-name (recipe-item-ingredient b)))))))
    (fold (lambda (item shopping-list)
    	    (cond ((null? shopping-list) (cons item shopping-list))
    		  ((string=? (ingredient-name (recipe-item-ingredient (car shopping-list)))
    			     (ingredient-name (recipe-item-ingredient item)))
		   (let ((res (add-recipe-items (car shopping-list) item)))
		     (cons res (cdr shopping-list))))
    		  (else (cons item shopping-list))))
    	  '()
    	  sorted)))

(pp (get-shopping-list-of recipes))



;; REPL

;;; Global environment for REPL.
(define the-global-environment
  'not-initialized)

(define the-empty-environment (list '*the-empty-environment*))

(define (make-global-environment)
  the-empty-environment)
  ;(extend-environment (map car initial-env-bindings)
  ;                    (map cdr initial-env-bindings)
  ;                    the-empty-environment))

(define (initialize-repl!)
  (set! the-global-environment (make-global-environment))
  'done)

(define (check-repl-initialized)
  (if (eq? the-global-environment 'not-initialized)
      (error "Interpreter not initialized. Run (init) first.")))

(define (g:read)
  (prompt-for-command-expression "eval> "))

(define (init)
  (initialize-repl!)
  (repl))

(define (go)
  (repl))

(define (g:eval exp env)
  'todo)

(define (repl)
  (check-repl-initialized)
  (let ((input (g:read)))
    (write-line (g:eval input the-global-environment))
    (repl)))


(init)
(repl)
