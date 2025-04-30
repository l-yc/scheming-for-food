(load "utils.scm")

;; define ingredient object
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


;; tests
(define chicken (make-simple-ingredient "chicken"))
(display chicken)


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

(define the-table (make-equal-hash-table))

(hash-table-set!
 the-table
 (cons (lookup-ingredient "scallions")
       (lookup-ingredient "green onion"))
 #t)

(hash-table-ref
 the-table
 (cons (lookup-ingredient "scallions")
       (lookup-ingredient "green onion")))

;(hash-table-intern!
;    (lookup-ingredient "scallions")
;    (lookup-ingredient "green onion")
; the-table

(pp hash-table-clear!)

;; these are related ingredients
(lookup-ingredient "garlic")
(lookup-ingredient "garlic cloves")
(lookup-ingredient "garlic powder")

;; these should be equal
(lookup-ingredient "scallions")
(lookup-ingredient "green onion")

(lookup-ingredient "heavy cream")
(lookup-ingredient "heavy whipping cream")

;; these should be tagged with protein
(lookup-ingredient "chicken thighs")
(lookup-ingredient "chicken breasts")
(lookup-ingredient "tofu")
(lookup-ingredient "soft tofu")
