;; READ !!!! Scuffed AF, but the way to get this file to work seems to be to
;; load data_loader, then recipe, then data_loader again. I'm pretty sure that
;; the issue comes from teh fact that both data_loader and recipe load the
;; same file, but I don't know why exactly this is an issue.
(load "data_loader.scm")
(load "recipe.scm")
(load "data_loader.scm")

;;; Utils

(define (list-all l)
  (fold-left (lambda (x y) (and x y)) #t l))

(define (list-any l)
  (fold-left (lambda (x y) (or x y)) #f l))

(define (list-count l)
  (cond
    ((null? l) 0)
    ((car l) (+ 1 (list-count (cdr l))))
    (else (list-count (cdr l)))))

(define (val-in-range? lo hi val)
  (and (<= lo val)
       (or (eqv? #f hi)
           (< val hi))))

;;; Ingredient predicates

;; Check whether an ingredient has a specific tag.
(define (is-tag tag)
  (list 'is-tag tag))

(define (not-tag condition)
  (list 'not-tag condition))

;; Check whether an ingredient meets *all* the listed conditions.
(define (all-tags . tags)
  (cons 'all-tags tags))

;; Check whether an ingredient meets *any* of the listed conditions.
(define (any-tags . tags)
  (cons 'any-tags tags))

;;; Recipe predicates

(define (all-ings-are-not condition)
 (list 'all-ings-are-not condition))

;; Checks whether *all* ingredients in the recipe meet *all* of the listed
;; criteria.
(define (all-ings-are-all-of . restrs)
  (cons 'all-ings-are-all-of restrs))

;; Checks whether *all* ingredients in the recipe meet *any* of the listed
;; criteria.
(define (all-ings-are-any-of . restrs)
  (cons 'all-ings-are-any-of restrs))

;; Checks if the number of conditions that are true is in [lo, hi). #f as hi
;; means no upper bound.
(define (ings-in-between lo hi . restrs)
  (cons 'ings-in-between (cons lo (cons hi restrs))))

(define (ings-at-most hi . restrs)
  (apply ings-in-between
         (cons 0 (cons (+ hi 1) restrs))))

(define (ings-at-least lo . restrs)
  (apply ings-in-between 
         (cons lo (cons #f restrs))))

;;; General combinators for combining recipe predicates

;; Checks whether the opposite condition is true for a set for a restriction
;; and a recipe.
(define (restr-does-not-apply restr)
  (list 'restr-does-not-apply restr))

;; Checks whether *all* restrictions apply to a certain recipe.
(define (all-restrs-apply . restrs)
  (cons 'all-restrs-apply restrs))

;; Checks whether *any* restrictions apply to a certain recipe.
(define (any-restr-applies . restrs)
  (cons 'any-restr-applies restrs))

;; Checks if the number of rules that are true is in [lo, hi). #f as hi
;; means no upper bound.
(define (restrictions-in-between lo hi . restrs)
  (cons 'restrictions-in-between (cons lo (cons hi restrs))))

(define (restrictions-at-most hi . restrs)
  (apply restrictions-in-between
         (cons 0 (cons (+ hi 1) restrs))))

(define (restrictions-at-least lo . restrs)
  (apply restrictions-in-between 
         (cons lo (cons #f restrs))))


;; Check if the given ingredient satisfies the given condition
(define (restr:check-ingredient query ingredient)
  (let ((tags (ingredient-tags ingredient)))
    (if (and (not (null? tags))
            (case (car query)
              ((is-tag) (any
                         (lambda (tag) (eqv? tag (cadr query)))
                         tags))
              ((all-tags) (list-all (map (lambda (q)
                                          (restr:check-ingredient q ingredient))
                                        (cdr query))))
              ((any-tags) (list-any (map (lambda (q)
                                          (restr:check-ingredient q ingredient))
                                      (cdr query))))
              ((not-tag) (not (restr:check-ingredient (cadr query) ingredient)))
              (else (error "restr:check-ingredient invalid query" query))))
      ingredient
      #f)))

;; Quick inline tests. TODO: move to other file
(define pep (ingredient-by-name "thai chili pepper fresh"))
(assert (restr:check-ingredient (is-tag 'spicy) pep))
(assert (restr:check-ingredient (not-tag (is-tag 'pork)) pep))
(assert (restr:check-ingredient (all-tags
                                  (is-tag 'spicy)
                                  (is-tag 'vegetable))
                               pep))
(assert (restr:check-ingredient (any-tags
                                  (is-tag 'pork) ;; false
                                  (is-tag 'vegetable))
                               pep))

;; Check if the given condition is true for *all* ingredients in a recipe.
(define (restr:applies-to-all-ingredients query recipe)
  (list-all (map (lambda (item)
                   (restr:check-ingredient
                     query
                     (recipe-item-ingredient item)))
                 (recipe-items recipe))))

;; Check if the given condition is true for *any* ingredients in a recipe.
(define (restr:applies-to-any-ingredients query recipe)
  (list-any (map (lambda (item)
                   (restr:check-ingredient
                     query
                     (recipe-item-ingredient item)))
                 (recipe-items recipe))))

;; Counts the number of ingredients for which the condition is true.
(define (restr:count-of-valid-ingredients query recipe)
  (list-count (map (lambda (item)
                     (restr:check-ingredient
                       query
                       (recipe-item-ingredient item)))
                   (recipe-items recipe))))

;; Checks if every a recipe satisfies the given condition.
(define (restr:check-recipe-rule query recipe)
  (let ((query-results (map (lambda (item)
                              (restr:check-ingredient
                                query
                                (recipe-item-ingredient item)))
                            (recipe-items recipe)))))
  (if (case (car query)
       ((all-ings-are-not) (not (restr:check-recipe-rule (cadr query) recipe)))
       ((all-ings-are-all-of) (restr:applies-to-all-ingredients (cadr query) recipe))
       ((all-ings-are-any-of) (restr:applies-to-any-ingredients (cadr query) recipe)) 
       ((ings-in-between) (val-in-range? (cadr query)
                                      (caddr query)
                                      (restr:count-of-valid-ingredients
                                         (cadddr query)
                                         recipe)))
       (else (error "restr:check-recipe-rule: invalid query" query)))
    recipe
    #f))

;; Quick inline tests. TODO: move to other file
(define test-spices
  (make-recipe
   "Test Spices"
   (list
    (make-recipe-item "black pepper" 1.67 'tbsp)
    (make-recipe-item "garlic powder" 3.33 'tbsp)
    (make-recipe-item "onion powder" 3.33 'tbsp)
    (make-recipe-item "paprika" 1.67 'tbsp))
   "da source"))
;; (#[ingredient 3 ("name" "black pepper") ("tags" (vegetable))]
;;  #[ingredient 6 ("name" "garlic powder") ("tags" (other))]
;;  #[ingredient 9 ("name" "onion powder") ("tags" (vegetable))]
;;  #[ingredient 12 ("name" "paprika") ("tags" (spice spicy))])

(assert (restr:check-recipe-rule
          (all-ings-are-all-of (not-tag (is-tag 'pork)))
          test-spices))
(assert (restr:check-recipe-rule
          (all-ings-are-any-of (is-tag 'spicy))
          test-spices))
(assert (restr:check-recipe-rule
          (ings-in-between 1 3 (is-tag 'vegetable))
          test-spices))
(assert (restr:check-recipe-rule
          (ings-at-most 2 (is-tag 'vegetable))
          test-spices))
(assert (restr:check-recipe-rule
          (ings-at-least 2 (is-tag 'vegetable))
          test-spices))

(define (restr:check-recipe query recipe)
  (if (case (car query)
       ((restr-does-not-apply) (not (restr:check-recipe-rule (cadr query) recipe)))
       ((all-restrs-apply) (list-all (map
                                      (lambda (q)
                                        (restr:check-recipe-rule q recipe))
                                      (cdr query))))
       ((any-restr-applies) (list-any (map
                                        (lambda (q)
                                          (restr:check-recipe-rule q recipe))
                                        (cdr query))))
       ((restrictions-in-between) (val-in-range?
                                    (cadr query)
                                    (caddr query)
                                    (list-count
                                      (map
                                        (lambda (q)
                                          (restr:check-recipe-rule q recipe))
                                        (cdddr query)))))
       (else (error "restr:check-recipe unknown query" (car query))))
    recipe
    #f))

;; Quick inline tests. TODO: move to other file
(define vegetarian
  (all-restrs-apply
    (all-ings-are-not (all-ings-are-any-of (any-tags (is-tag 'pork))
                                   (is-tag 'beef)
                                   (is-tag 'chicken)))
    (all-ings-are-not (all-ings-are-any-of (any-tags (is-tag 'shellfish))
                                          (is-tag 'fish)))))
(assert (restr:check-recipe vegetarian test-spices))

(define halal
  (all-restrs-apply
    (all-ings-are-all-of (not-tag (is-tag 'pork)))))
(assert (restr:check-recipe halal test-spices))

(define only-pork
  (all-restrs-apply
    (all-ings-are-all-of (is-tag 'pork))))
(assert (not (restr:check-recipe only-pork test-spices)))

(define a-non-kosher-meal
  (make-recipe
   "this is not kosher"
   (list
    (make-recipe-item "milk" 1.67 'tbsp)
    (make-recipe-item "pork tenderloin" 1.67 'tbsp))
   "definitely not the torah"))
(define kosher
  (restrictions-at-most 1
                 (all-ings-are-any-of (is-tag 'dairy))
                 (all-ings-are-any-of (is-tag 'pork))))
(assert (restr:check-recipe kosher test-spices))
(assert (not (restr:check-recipe kosher a-non-kosher-meal)))
