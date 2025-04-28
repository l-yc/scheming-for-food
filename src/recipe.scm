(load "utils.scm")
(load "ingredient.scm")
(load "units.scm")

;; define recipe
(define-record-type <recipe-item>
  (%make-recipe-item ingredient quantity)
  recipe-item?
  (ingredient recipe-item-ingredient)
  (quantity recipe-item-quantity))

(define (recipe? x)
  (and (list? x)
       (every recipe-item? x)))

;; tests

;; simple example
(define rice (make-simple-ingredient "rice"))
(define beans (make-simple-ingredient "beans"))

(define rice-and-beans-recipe
 (list 
  (%make-recipe-item rice (make-quantity 1 'cup))
  (%make-recipe-item beans (make-quantity 1 'cup))))

(pp rice-and-beans-recipe)
