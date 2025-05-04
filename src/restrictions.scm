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
(define (ing-is tag)
  (list 'ing-is tag))

(define (ing-not condition)
  (list 'ing-not condition))

;; Check whether an ingredient meets *all* the listed conditions.
(define (ing-all . tags)
  (cons 'ing-all tags))

;; Check whether an ingredient meets *any* of the listed conditions.
(define (ing-any . tags)
  (cons 'ing-any tags))

;;; Recipe predicates

(define (recip-not condition)
 (list 'recip-not condition))

;; Checks whether *all* ingredients in the recipe meet *all* of the listed
;; criteria.
(define (recip-all . restrs)
  (cons 'recip-all restrs))

;; Checks whether *all* ingredients in the recipe meet *any* of the listed
;; criteria.
(define (recip-any . restrs)
  (cons 'recip-any restrs))

;; Checks if the number of conditions that are true is in [lo, hi). #f as hi
;; means no upper bound.
(define (recip-n-true lo hi . restrs)
  (cons 'recip-n-true (cons lo (cons hi restrs))))

(define (recip-at-most hi . restrs)
  (apply recip-n-true
         (cons 0 (cons (+ hi 1) restrs))))

(define (recip-at-least lo . restrs)
  (apply recip-n-true 
         (cons lo (cons #f restrs))))

;;; General combinators for combining recipe predicates

;; Checks whether the opposite condition is true for a set for a restriction
;; and a recipe.
(define (restr-not restr)
  (list 'restr-not restr))

;; Checks whether *all* restrictions apply to a certain recipe.
(define (restr-all . restrs)
  (cons 'restr-all restrs))

;; Checks whether *any* restrictions apply to a certain recipe.
(define (restr-any . restrs)
  (cons 'restr-any restrs))

;; Check if the given ingredient satisfies the given condition
(define (restr:check-ingredient query ingredient)
  (let ((tags (ingredient-tags ingredient)))
    (and (not (null? tags))
         (case (car query)
           ((ing-is) (any
                      (lambda (tag) (eqv? tag (cadr query)))
                      tags))
           ((ing-all) (list-all (map (lambda (q)
                                       (restr:check-ingredient q ingredient))
                                     (cdr query))))
           ((ing-any) (list-any (map (lambda (q)
                                       (restr:check-ingredient q ingredient))
                                   (cdr query))))
           ((ing-not) (not (restr:check-ingredient (cadr query) ingredient)))
           (else (error "restr:check-ingredient invalid query" query))))))

;; Quick inline tests. TODO: move to other file
(define pep (ingredient-by-name "thai chili pepper fresh"))
(assert (restr:check-ingredient (ing-is 'spicy) pep))
(assert (restr:check-ingredient (ing-not (ing-is 'pork)) pep))
(assert (restr:check-ingredient (ing-all
                                  (ing-is 'spicy)
                                  (ing-is 'vegetable))
                               pep))
(assert (restr:check-ingredient (ing-any
                                  (ing-is 'pork) ;; false
                                  (ing-is 'vegetable))
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
  (case (car query)
    ((recip-not) (not (restr:check-recipe-rule (cadr query) recipe)))
    ((recip-all) (restr:applies-to-all-ingredients (cadr query) recipe))
    ((recip-any) (restr:applies-to-any-ingredients (cadr query) recipe)) 
    ((recip-n-true) (val-in-range? (cadr query)
                                   (caddr query)
                                   (restr:count-of-valid-ingredients
                                     (cadddr query)
                                     recipe)))
    (else (error "restr:check-recipe-rule: invalid query" query))))

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
          (recip-all (ing-not (ing-is 'pork)))
          test-spices))
(assert (restr:check-recipe-rule
          (recip-any (ing-is 'spicy))
          test-spices))
(assert (restr:check-recipe-rule
          (recip-n-true 1 3 (ing-is 'vegetable))
          test-spices))
(assert (restr:check-recipe-rule
          (recip-at-most 2 (ing-is 'vegetable))
          test-spices))
(assert (restr:check-recipe-rule
          (recip-at-least 2 (ing-is 'vegetable))
          test-spices))

(define (restr:check-recipe query recipe)
  (case (car query)
    ((restr-not) (not (restr:check-recipe-rule (cadr query) recipe)))
    ((restr-all) (list-all (map
                             (lambda (q)
                               (restr:check-recipe-rule q recipe))
                             (cdr query))))
    ((restr-any) (list-any (map
                             (lambda (q)
                               (restr:check-recipe-rule q recipe))
                             (cdr query))))
    (else (error "restr:check-recipe unknown query" (car query)))))

;; Quick inline tests. TODO: move to other file

(define vegetarian
  (restr-all
    (recip-not (recip-any (ing-any (ing-is 'pork)
                                   (ing-is 'beef)
                                   (ing-is 'chicken))))
    (recip-not (recip-any (ing-any (ing-is 'shellfish)
                                   (ing-is 'fish))))))
(assert (restr:check-recipe vegetarian test-spices))

(define halal
  (restr-all
    (recip-all (ing-not (ing-is 'pork)))))
(assert (restr:check-recipe halal test-spices))

(define only-pork
  (restr-all
    (recip-all (ing-is 'pork))))
(assert (not (restr:check-recipe only-pork test-spices)))

;;; Some Example Queries
;; TODO: kosher, balanced
