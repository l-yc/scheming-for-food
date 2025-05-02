(load "utils.scm")

;;; Ingredients

(define-record-type <ingredient>
  (%make-ingredient canonical-name names tags)
  ingredient?
  (canonical-name ingredient-name)
  (names ingredient-aliases)
  (tags ingredient-tags))

(define-print-method ingredient?
  (standard-print-method
   "ingredient"
   (lambda (i)
     `(("name" ,(ingredient-name i))))))

(define (make-simple-ingredient name)
  (make-ingredient name '() '()))

(define (make-ingredient canonical-name names tags)
  (assert (string? canonical-name))
  (assert (and (list? names)
               (every string? names)))
  (assert (and (list? tags)
           (every symbol? tags)))
  (%make-ingredient canonical-name names tags))

;; define ingredients relations

;; TODO think about how to implement ingredients
;; equality,
;; a can be turned into b substitution

;; maybe worth it to start on query engine before to see
;; what are more natural relationships to model


;(define (set-tag<=! tag superset)
;  (if (tag>= tag superset)
;      (error "Not allowed to create a superset loop:"
;             tag superset))
;  (if (not (tag<= tag superset))
;      (((tag-supersets tag) 'add-element!) superset))
;  (hash-table-clear! tag<=-cache))

;(define the-table (make-equal-hash-table))
;
;(hash-table-set!
; the-table
; (cons (ingredient-by-name "scallions")
;       (ingredient-by-name "green onion"))
; #t)
;
;(hash-table-ref
; the-table
; (cons (ingredient-by-name "scallions")
;       (ingredient-by-name "green onion")))
;
;;(hash-table-intern!
;;    (ingredient-by-name "scallions")
;;    (ingredient-by-name "green onion")
;; the-table
;
;;; these are related ingredients
;(ingredient-by-name "garlic")
;(ingredient-by-name "garlic cloves")
;(ingredient-by-name "garlic powder")
;
;;; these should be equal
;(ingredient-by-name "scallions")
;(ingredient-by-name "green onion")
;
;(ingredient-by-name "heavy cream")
;(ingredient-by-name "heavy whipping cream")
;
;;; these should be tagged with protein
;(ingredient-by-name "chicken thighs")
;(ingredient-by-name "chicken breasts")
;(ingredient-by-name "tofu")
;(ingredient-by-name "soft tofu")
