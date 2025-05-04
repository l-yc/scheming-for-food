(load "utils.scm")
(load "units.scm")

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
	(q1 (canonicalize-quantity (recipe-item-quantity i1)))
	(q2 (canonicalize-quantity (recipe-item-quantity i2))))
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
		   (begin
		   (let ((res (add-recipe-items (car shopping-list) item)))
		     (cons res (cdr shopping-list)))
		   )

		   )
    		  (else (cons item shopping-list))))
    	  '()
    	  sorted)))


(pp (get-shopping-list-of recipes))
