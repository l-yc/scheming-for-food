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

(define (contains-key? table key)
  (not (eq? '#f (hash-table-ref table key (lambda () #f)))))

(define (hash-table-set-if-not-present! table key val)
  (if (contains-key? table key)
    (error "hash-table-set-if-not-present! key" key "is already present")
    (hash-table-set! table key val))
  'ok)

;;; Typeclass system

(define (query? x)
  (or (symbol? x)
      (and (list? x)
           (symbol? (car x)))))

(define (query-tag x)
  (if (symbol? x)
    x
    (car x)))

(define query-vtable (make-strong-eqv-hash-table 10))

(define (register-all-query! query)
  (assert (query? query) "query must be a query; got" query)
  (hash-table-set-if-not-present! query-vtable (query-tag query) 'all))

(define (register-any-query! query)
  (assert (query? query) "query must be a query; got" query)
  (hash-table-set-if-not-present! query-vtable (query-tag query) 'any))

(define (register-range-query! query)
  (assert (query? query) "query must be a query; got" query)
  (hash-table-set-if-not-present! query-vtable (query-tag query) 'range))

(define (do-query query f data)
  (assert (query? query) "query must be a query; got" query)
  (cond
    ((symbol? query) (do-query-inner (query-tag query) f data))
    ((list? query) (apply do-query-inner
                          (cons (query-tag query) (cons f (cons data (cdr query))))))
    (else (error "do-query: unknown query" query))))

(define (do-query-inner query-tag f data . extra-args) 
  (case (hash-table-ref query-vtable query-tag)
    ((all) (list-all (map f data)))
    ((any) (list-any (map f data)))
    ((range) (let ((lo (car extra-args)) (hi (cadr extra-args)))
               (assert (number? (car extra-args))) ;; lo
               (assert ((disjoin number? boolean?) (cadr extra-args))) ;; hi
               (val-in-range? lo hi
                              (list-count (map f data))))) ;; hi
    (else (error "handle-query: unknown query tag" query-tag))))

;;; Ingredient predicates

;; Check whether an ingredient has a specific tag.
(define (is-tag tag)
  (list 'is-tag tag))

(define (not-tag condition)
  (list 'not-tag condition))

;; Check whether an ingredient meets *all* the listed conditions.
(define (all-tags . tags)
  (cons 'all-tags tags))
(register-all-query! 'all-tags)

;; Check whether an ingredient meets *any* of the listed conditions.
(define (any-tags . tags)
  (cons 'any-tags tags))
(register-any-query! 'any-tags)

;; Check whether an ingredient meets *between lo and hi* of the listed
;; conditions.
(define (tags-in-between lo hi . restrs)
  (cons (list 'tags-in-between lo hi) restrs))
(register-range-query! 'tags-in-between)

;; Check whether an ingredient meets *at most hi* of the listed conditions.
(define (tags-at-most hi . restrs)
  (apply tags-in-between
         (cons 0 (cons (+ hi 1) restrs))))

;; Check whether an ingredient meets *at least lo* of the listed conditions.
(define (tags-at-least lo . restrs)
  (apply tags-in-between 
         (cons lo (cons #f restrs))))

;;; Recipe predicates

(define (all-ings-are-not condition)
 (list 'all-ings-are-not condition))

;; Checks whether *all* ingredients in the recipe meet *all* of the listed
;; criteria.
(define (all-ings-are-all-of . restrs)
  (cons 'all-ings-are-all-of restrs))
(register-all-query! 'all-ings-are-all-of)

;; Checks whether *all* ingredients in the recipe meet *any* of the listed
;; criteria.
(define (all-ings-are-any-of . restrs)
  (cons 'all-ings-are-any-of restrs))
(register-any-query! 'all-ings-are-any-of)

;; Checks if the number of conditions that are true is in [lo, hi). #f as hi
;; means no upper bound.
(define (ings-in-between lo hi . restrs)
  (cons (list 'ings-in-between lo hi) restrs))
(register-range-query! 'ings-in-between)

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
(register-all-query! 'all-restrs-apply)

;; Checks whether *any* restrictions apply to a certain recipe.
(define (any-restr-applies . restrs)
  (cons 'any-restr-applies restrs))
(register-any-query! 'any-restr-applies)

;; Checks if the number of rules that are true is in [lo, hi). #f as hi
;; means no upper bound.
(define (restrs-in-between lo hi . restrs)
  (cons (list 'restrs-in-between lo hi) restrs))
(register-range-query! 'restrs-in-between)

(define (restrs-at-most hi . restrs)
  (apply restrs-in-between
         (cons 0 (cons (+ hi 1) restrs))))

(define (restrs-at-least lo . restrs)
  (apply restrs-in-between 
         (cons lo (cons #f restrs))))

;; Check if the given ingredient satisfies the given condition
(define (restr:check-ingredient query ingredient)
  (let ((tags (ingredient-tags ingredient)))
    (and (not (null? tags))
         (case (query-tag (car query))
           ((is-tag) (member (cadr query) tags))
           ((not-tag) (not (restr:check-ingredient (cadr query) ingredient)))
           ((all-tags any-tags tags-in-between)
            (do-query
              (car query)
              (lambda (q)
                (restr:check-ingredient q ingredient))
              (cdr query)))
           (else (error "restr:check-ingredient invalid query" query))))))

(define (restr:check-recipe-rule query recipe)
  (case (query-tag (car query))
    ((all-ings-are-not) (not (restr:check-recipe-rule (cadr query) recipe)))
    ((all-ings-are-all-of all-ings-are-any-of ings-in-between)
     (do-query
       (car query)
       (lambda (item)
               (restr:check-ingredient (cadr query) (recipe-item-ingredient item)))
       (recipe-items recipe)))
    (else (error "restr:check-recipe-rule: invalid query" query))))


(define (restr:check-recipe query recipe)
  (case (query-tag (car query))
    ((restr-does-not-apply) (not (restr:check-recipe-rule (cadr query) recipe)))
    ((all-restrs-apply any-restr-applies restrs-in-between)
     (do-query
      (car query)
      (lambda (q) (restr:check-recipe-rule q recipe))
      (cdr query)))
    (else (error "restr:check-recipe unknown query" (car query)))))

;;; Tests: TODO move to other file

;; Ingredient level tests
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
(assert (restr:check-ingredient (tags-at-least 1
                                  (is-tag 'pork) ;; false
                                  (is-tag 'vegetable))
                               pep))
(assert (restr:check-ingredient (tags-at-most 1
                                  (is-tag 'pork) ;; false
                                  (is-tag 'vegetable))
                               pep))
(assert (restr:check-ingredient (tags-in-between 0 2
                                  (is-tag 'pork) ;; false
                                  (is-tag 'vegetable))
                               pep))
(assert (not (restr:check-ingredient (tags-in-between 0 1
                                       (is-tag 'pork) ;; false
                                       (is-tag 'vegetable))
                                   pep)))

;; Restriction level tests
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

;; end to end, multiple restrictions
(define vegetarian
  (all-restrs-apply
    (all-ings-are-not (all-ings-are-any-of (any-tags (is-tag 'pork)
                                                     (is-tag 'beef)
                                                     (is-tag 'chicken)
                                                     (is-tag 'poultry))))
    (all-ings-are-not (all-ings-are-any-of (any-tags (is-tag 'shellfish)
                                                     (is-tag 'fish))))))
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
  (restrs-at-most 1
                 (all-ings-are-any-of (is-tag 'dairy))
                 (all-ings-are-any-of (is-tag 'pork))))
(assert (restr:check-recipe kosher test-spices))
(assert (not (restr:check-recipe kosher a-non-kosher-meal)))
