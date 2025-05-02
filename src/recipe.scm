(load "utils.scm")
(load "ingredient.scm")
(load "data_loader.scm")
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
(expect (linear-scaling-rule? (cons 'linear 'a)) #t)

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
    (let ((ingredient (lookup-ingredient name)))
     (assert ingredient "recipe contains unknown ingredient:" name)
     (%make-recipe-item ingredient (make-quantity amount unit) rule)))

  (let ((nargs (length args)))
    (assert (or (= nargs 3)
                (= nargs 4))
            "make-recipe-item should be called with 3 or 4 args \
            (the 4th is an optional scaling rule)")
    (apply the-constructor
     (if (= l 3)
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
  (guarantee string? name)
  (guarantee list? items)
  (guarantee (lambda (lst) (every recipe-item? lst)) items)
  (%make-recipe name items provenance))

;; tests

;; simple example
(define rice (make-simple-ingredient "rice"))
(define beans (make-simple-ingredient "beans"))

(define rice-and-beans-recipe
 (list 
  (%make-recipe-item rice (make-quantity 1 'cup) "none")
  (%make-recipe-item beans (make-quantity 1 'cup) "none")))

;; examples from german house meal plan website
;; https://dh.mit.edu/menu/meal/2880/

(define crispy-baked-chicken-thighs-recipe
  (make-recipe
   "Crispy Baked Chicken Thighs"
   (list
    (make-recipe-item "black pepper" 1.67 'tbsp)
    (make-recipe-item "chicken thighs" 12 'lb)
    ;(make-recipe-item "Cornstarch" 0.62 'cup)
    (make-recipe-item "cornstarch" 0.62 'cup)
    (make-recipe-item "garlic powder" 3.33 'tbsp)
    (make-recipe-item "olive oil" 0.62 'cup)
    (make-recipe-item "onion powder" 3.33 'tbsp)
    (make-recipe-item "paprika" 1.67 'tbsp)
    (make-recipe-item "salt" 3.33 'tbsp)
    (make-recipe-item "tofu" 1 'lb))
    
   "https://dh.mit.edu/recipes/928/"))

(define penne-cinque-pi-recipe
  (make-recipe
   "Penne Cinque Pi"
   (list
    (make-recipe-item "nutmeg" 4 'pinch)
    (make-recipe-item "penne" 5.51 'lb)
    (make-recipe-item "single cream" 5.92 'cup)
    (make-recipe-item "tomato paste" 0.66 'lb)
    (make-recipe-item "parmesan" 0.66 'lb)
    (make-recipe-item "parsley" 0.62 'cup)
    (make-recipe-item "salt" 0 'pinch)
    (make-recipe-item "pepper" 0 'pinch)
    (make-recipe-item "tomato pasta sauce" 0.5 'jar))
   "https://dh.mit.edu/recipes/1465/"))

(define spicy-roasted-green-beans-recipe
  (make-recipe
   "spicy roasted green beans"
   (list
    (make-recipe-item "green beans" 10 'lb)
    (make-recipe-item "sesame oil" 0.47 'cup)
    (make-recipe-item "salt" 10 'taste)
    (make-recipe-item "black pepper" 10 'taste)
    (make-recipe-item "garlic" 30 'clove)
    (make-recipe-item "red pepper flakes" 0.83 'tbsp)
    (make-recipe-item "soy sauce" 0.62 'cup)
    (make-recipe-item "brown sugar" 1.25 'cup)
    (make-recipe-item "ground ginger" 0.83 'tbsp))
   "https://dh.mit.edu/recipes/1462/"))

(define recipes
  (list crispy-baked-chicken-thighs-recipe
   penne-cinque-pi-recipe
   spicy-roasted-green-beans-recipe))
