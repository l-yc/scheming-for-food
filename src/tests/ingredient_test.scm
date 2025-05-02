(load "ingredient.scm")
(load "recipe.scm")
(load "data_loader.scm")

;; tests
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
