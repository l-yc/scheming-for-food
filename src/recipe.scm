(load "utils.scm")
(load "ingredient.scm")
(load "data_loader.scm")
;(load "units.scm")

;; define recipe
(define-record-type <recipe-item>
  (%make-recipe-item ingredient quantity)
  recipe-item?
  (ingredient recipe-item-ingredient)
  (quantity recipe-item-quantity))

(define-print-method recipe-item?
  (standard-print-method
   "recipe-item"
   (lambda (ri)
     `((,(recipe-item-ingredient ri) ,(recipe-item-quantity ri))))))


(define-record-type <recipe>
  (%make-recipe name items)
  recipe?
  (name recipe-name)
  (items recipe-items))

(define-print-method recipe?
  (standard-print-method
   "recipe"
   (lambda (r)
     `(("name" ,(recipe-name r))))))

(define (make-recipe name items)
  (guarantee string? name)
  (guarantee list? items)
  (guarantee (lambda (lst) (every recipe-item? lst)) items)
  (%make-recipe name items))




(define (lookup-ingredient name)
  (assert (string? name))
  (find (lambda (ingredient)
	  (string=? (ingredient-name ingredient) name))
	ingredients-list))

(define (make-recipe-item name amount unit)
  (assert (string? name))
  (assert (number? amount))
  (assert (unit? unit))
  (let ((ingredient (lookup-ingredient name)))
    (assert ingredient "recipe contains unknown ingredient:" name)
    (%make-recipe-item ingredient (make-quantity amount unit))))

;; tests

;; simple example
(define rice (make-simple-ingredient "rice"))
(define beans (make-simple-ingredient "beans"))

(define rice-and-beans-recipe
 (list 
  (%make-recipe-item rice (make-quantity 1 'cup))
  (%make-recipe-item beans (make-quantity 1 'cup))))

(pp rice-and-beans-recipe)


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
    (make-recipe-item "tofu" 1 'lb)
   )))

(define penne-cinque-pi-recipe
  (make-recipe
   "Penne Cinque Pi"
   (list
    ;(make-recipe-item "Nutmeg" 4 'pinch)
    (make-recipe-item "nutmeg" 4 'pinch)
    ;(make-recipe-item "Penne" 5.51 'lb)
    (make-recipe-item "penne" 5.51 'lb)
    ;(make-recipe-item "Single cream (approx. 25% fat)" 5.92 'cup)
    (make-recipe-item "single cream" 5.92 'cup)
    ;(make-recipe-item "Tomato paste" 0.66 'lb)
    (make-recipe-item "tomato paste" 0.66 'lb)
    ;(make-recipe-item "Parmesan" 0.66 'lb)
    (make-recipe-item "parmesan" 0.66 'lb)
    ;(make-recipe-item "Parsley" 0.62 'cup)
    (make-recipe-item "parsley" 0.62 'cup)
    ;(make-recipe-item "Salt" 0 'pinch)
    (make-recipe-item "salt" 0 'pinch)
    ;(make-recipe-item "Pepper" 0 'pinch)
    (make-recipe-item "pepper" 0 'pinch)
    ;(make-recipe-item "Tomato pasta sauce" 0.5 'jar)
    (make-recipe-item "tomato pasta sauce" 0.5 'jar)
   )))

(define spicy-roasted-green-beans-recipe
  (make-recipe
   "spicy roasted green beans"
   (list
    (make-recipe-item "green beans" 10 'lb)
    (make-recipe-item "sesame oil" 0.47 'cup)
    (make-recipe-item "salt" 10 'taste)
    (make-recipe-item "black pepper" 10 'taste)
    (make-recipe-item "garlic" 30 'clove)
    ;(make-recipe-item "red pepper flakes (may want to cut down on this)" 0.83 'tbsp)
    (make-recipe-item "red pepper flakes" 0.83 'tbsp)
    (make-recipe-item "soy sauce" 0.62 'cup)
    (make-recipe-item "brown sugar" 1.25 'cup)
    (make-recipe-item "ground ginger" 0.83 'tbsp)
   )))

(define recipes
  (list crispy-baked-chicken-thighs-recipe
	penne-cinque-pi-recipe
	spicy-roasted-green-beans-recipe))
