(load "data_loader.scm")
(load "recipe.scm")

(define (restriction? obj)
  (and (list obj)
       (not (null? obj))
       (symbol? (car obj))
       (every (disjoin symbol? restriction?) obj)))

;;; Ingredient predicates

(define (ing-is tag)
  (list 'ing-is tag))
(define (ing-and . tags)
  (cons 'ing-and tags))
(define (ing-or . tags)
  (cons 'ing-or tags))

;;; Recipe predicates

(define (recip-all . restrs)
  (cons 'recip-all restrs))
(define (recip-any . restrs)
  (cons 'recip-any restrs))
(define (recip-only-one . restrs)
  (cons 'recip-only-one restrs))

;;; General Predicates
(define (restr-not restr)
  (list 'not restr))

;; TODO: add general restr-and and restr-or, maybe rename recip-all to recip-and
;; and recip-any to recip-or and recip-only-one to recip-xor

;; TODO: disjoint: milk and meat are disjoint

;;; Some Example Queries

(define (restriction:halal)
  (restr-not (recip-any (ing-is 'pork))))

(define (restriction:vegetarian)
  (restr-not (recip-any (ing-or (ing-is 'pork)
                                (ing-is 'beef)
                                (ing-is 'chicken)
                                (ing-is 'fish)
                                (ing-is 'shellfish)))))

(define (and2 x y) (and x y))
(define (or2 x y) (or x y))

(define (list:and l)
  (fold-left and2 #t l))

(define (list:or l)
  (fold-left or2 #f l))

(define (restr:check-ingredient query ingredient)
  (let ((tags (ingredient-tags ingredient)))
    (and (not (null? tags))
         (case (car query)
           ((ing-is) (any
                      (lambda (tag) (eqv? tag (cadr query)))
                      tags))
           ((ing-and) (list:and (map (lambda (q)
                                       (restr:check-ingredient q ingredient))
                                     (cdr query))))
           ((ing-or) (list:or (map (lambda (q)
                                     (restr:check-ingredient q ingredient))
                                   (cdr query))))
           (else (error "restr:check-ingredient invalid query" query))))))

;; Quick inline tests. TODO: move to other file
(define pep (ingredient-by-name "thai chili pepper fresh"))
(assert (restr:check-ingredient (ing-is 'spicy) pep))
(assert (restr:check-ingredient (ing-and
                                  (ing-is 'spicy)
                                  (ing-is 'vegetable))
                               pep))
(assert (restr:check-ingredient (ing-or
                                  (ing-is 'pork) ;; false
                                  (ing-is 'vegetable))
                               pep))

(define (restr:applies-to-all-ingredients query recipe)
  (list:and (map (lambda (item)
                   (restr:check-ingredient
                     query
                     (recipe-item-ingredient item)))
                 (recipe-items recipe))))

(define (restr:applies-to-any-ingredients query recipe)
  (list:or (map (lambda (item)
                  (restr:check-ingredient
                    query
                    (recipe-item-ingredient item)))
                (recipe-items recipe))))

(define (restr:check-recipe-rule query recipe)
  (case (car query)
    ((recip-all) (restr:applies-to-all-ingredients (cadr query) recipe))
    ((recip-any) (restr:applies-to-any-ingredients (cadr query) recipe)) 
    ((recip-only-one) (assert #f "unimplemented"))
    (else (error "restr:check-recipe-rule: invalid query" query))))
