(load "restrictions.scm")
(load "data/ingredients_sorted_tagged_nodup.scm")

;; A vegetarian meal

(define buttermilk-fried-chicken
  (make-recipe
   "Buttermilk Fried Chicken"
   (list
    (make-recipe-item "black pepper" 1.67 'tbsp)
    (make-recipe-item "buttermilk" 1.0 'cup)
    (make-recipe-item "chicken thighs" 12 'lb)
    (make-recipe-item "cornstarch" 0.62 'cup)
    (make-recipe-item "garlic powder" 3.33 'tbsp)
    (make-recipe-item "olive oil" 0.62 'cup)
    (make-recipe-item "onion powder" 3.33 'tbsp)
    (make-recipe-item "paprika" 1.67 'tbsp)
    (make-recipe-item "salt" 3.33 'tbsp)
    (make-recipe-item "tofu" 1 'lb))
   "da source"))

;; A simple example of composition

(define meat-ingredient
  (any-tags (is-tag 'pork)
            (is-tag 'beef)
            (is-tag 'chicken)
            (is-tag 'poultry)))

(define seafood-ingredient
  (any-tags (is-tag 'shellfish)
            (is-tag 'fish)))

;; vegetarian is defined as having no meat or shellfish
(define vegetarian
  (all-restrs-apply
    (ings-not (any-ings-are meat-ingredient))
    (ings-not (any-ings-are seafood-ingredient))))

(restr:check-recipe vegetarian buttermilk-fried-chicken)
; #f

;; spicy is defined as having at least 2 spices that are spicy
(define spicy
  (all-restrs-apply
    (ings-at-least 2 (all-tags
                       (is-tag 'spicy)
                       (is-tag 'spice)))))

(restr:check-recipe spicy buttermilk-fried-chicken)
; #t

(define high-protein-ingredient
  (any-tags
    (is-tag 'pork)
    (is-tag 'beef)
    (is-tag 'chicken)
    (is-tag 'poultry)
    (is-tag 'legume)))

;; high-protein is defined as having 2 or more high protein ingredients
(define high-protein
  (all-restrs-apply
    (ings-at-least 2 high-protein-ingredient)))

(restr:check-recipe high-protein buttermilk-fried-chicken)
; #t

;; halal is defined as no pork
(define halal
  (all-restrs-apply
    (all-ings-are (not-tag (is-tag 'pork)))))
(restr:check-recipe halal buttermilk-fried-chicken)
; #t

;; kosher is defined as not having at most 1 or meat or milk
(define kosher
  (restrs-at-most 1
                 (any-ings-are (is-tag 'dairy))
                 (any-ings-are meat-ingredient)))
(restr:check-recipe kosher buttermilk-fried-chicken)
; #f
