(load "utils.scm")
(load "ingredient.scm")
(load "units.scm")

;;; Scaling Rules

(define (constant-scaling-rule? r) (eq? 'constant r))
(define (linear-scaling-rule? r)
  (and (pair? r)
       (eq? 'linear (car r))
       (number? (cdr r))))
(define (scaling-rule? r)
  (or (constant-scaling-rule? r)
      (linear-scaling-rule? r)))

(expect (constant-scaling-rule? 'constant) #t)
(expect (constant-scaling-rule? 'linear) #f)
(expect (linear-scaling-rule? (cons 'linear 2)) #t)
(expect (linear-scaling-rule? (cons 'linear 'a)) #f)

;;; Recipe Items

(define-record-type <recipe-item>
  (%make-recipe-item ingredient quantity scaling-rule)
  recipe-item?
  (ingredient recipe-item-ingredient)
  (quantity recipe-item-quantity)
  (scaling-rule recipe-item-scaling-rule))

(define-print-method recipe-item?
  (standard-print-method
   "recipe-item"
   (lambda (ri)
     `((,(recipe-item-ingredient ri) ,(recipe-item-quantity ri))))))

(define (make-recipe-item . args)
  (define (the-constructor name amount unit rule)
    (assert (string? name))
    (assert (number? amount))
    (assert (unit? unit))
    (assert (scaling-rule? rule))
    (let ((ingredient (ingredient-by-name name)))
     (assert ingredient "recipe contains unknown ingredient:" name)
     (%make-recipe-item ingredient (make-quantity amount unit) rule)))

  (let ((nargs (length args)))
    (assert (or (= nargs 3)
                (= nargs 4))
            "make-recipe-item should be called with 3 or 4 args \
            (the 4th is an optional scaling rule)")
    (apply the-constructor
     (if (= nargs 3)
         (append args (list (cons 'linear 1)))
         args))))

;;; Recipes

(define-record-type <recipe>
  (%make-recipe name items provenance)
  recipe?
  (name recipe-name)
  (items recipe-items)
  (provenance recipe-provenance))

(define-print-method recipe?
  (standard-print-method
   "recipe"
   (lambda (r)
     `(("name" ,(recipe-name r))))))

(define (make-recipe name items provenance)
  (assert (string? name))
  (assert (list? items))
  (assert ((lambda (lst) (every recipe-item? lst)) items))
  (%make-recipe name items provenance))
