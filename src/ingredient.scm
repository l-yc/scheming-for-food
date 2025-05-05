(load "utils.scm")

;;; Ingredients

(define-record-type <ingredient>
  (%make-ingredient canonical-name tags)
  ingredient?
  (canonical-name ingredient-name)
  (tags ingredient-tags set-ingredient-tags!))

(define-print-method ingredient?
  (standard-print-method
   "ingredient"
   (lambda (i)
     `(("name" ,(ingredient-name i))
       ("tags" ,(ingredient-tags i))))))

(define (make-simple-ingredient name)
  (make-ingredient name '()))

(define (make-ingredient canonical-name tags)
  (assert (string? canonical-name))
  (assert (and (list? tags)
           (every symbol? tags)))
  (%make-ingredient canonical-name tags))

;; tags thing inspired by sdf
(define tag<=table
  (make-equal-hash-table))


(define tag<=-cache
  (make-equal-hash-table))


(define (get-tag-supersets tag)
  (hash-table-intern! tag<=table
		      tag
		      (lambda () '())))

  
(define (set-tag<=! tag superset)
  (if (tag>= tag superset)
      (error "Not allowed to create a superset loop:"
             tag superset))
  (if (not (tag<= tag superset))
      (hash-table-set! tag<=table
		       tag
		       (cons superset
			     (get-tag-supersets tag))))
  (hash-table-clear! tag<=-cache))


(define (cached-tag<= tag1 tag2)
  (hash-table-intern! tag<=-cache
		      (cons tag1 tag2)
		      (lambda () (uncached-tag<= tag1 tag2))))


(define (uncached-tag<= tag1 tag2)
  (or (eqv? tag1 tag2)
      (any (lambda (tag)
             (cached-tag<= tag tag2))
           (get-tag-supersets tag1))))


(define (tag? t) (symbol? t))


(define (tag<= tag1 tag2)
  (assert (tag? tag1))
  (assert (tag? tag2))
  (cached-tag<= tag1 tag2))


(define (tag>= tag1 tag2)
  (tag<= tag2 tag1))


;; tests
(set-tag<=! 'meat 'protein)
(set-tag<=! 'legume 'protein)
(set-tag<=! 'tofu 'legume)
(expect (tag<= 'meat 'protein) #t)
(expect (tag<= 'tofu 'protein) #t)
(expect (tag>= 'tofu 'protein) #f)
(expect (tag>= 'tofu 'meat) #f)
(expect (tag<= 'tofu 'meat) #f)



;; define ingredients relations

;; the idea for equality between ingredients is that
;; they will share special tags that are "identity tags",
;; i.e. tags that can identify different ingredients as the same thing

(define %tag-counter 0)
(define (new-tag)
  (string->symbol (string-append "tag-autogen-"
				 (number->string (set! %tag-counter
						       (+ %tag-counter 1))))))

(define %identity-tag '%tag-id)
(define (identity-tag? t) (tag<= t %identity-tag))
(define (set-identity-tag! t) (set-tag<=! t %identity-tag))

(set-identity-tag! 'scallion)

(define (ingredient= i1 i2)
  (assert (ingredient? i1))
  (assert (ingredient? i2))
  (let ((tags1 (ingredient-tags i1))
	(tags2 (ingredient-tags i2)))
    (not (not (any (lambda (t) (member t tags2))
		   (filter (lambda (t) (identity-tag? t))
			   tags1))))))

(define (set-ingredient=! i1 i2)
  (assert (ingredient? i1))
  (assert (ingredient? i2))
  (if (not (ingredient= i1 i2))
      (let ((tags1 (ingredient-tags i1))
	    (tags2 (ingredient-tags i2))
	    (ing-tag (new-tag)))

	(set-identity-tag! ing-tag)
	(set-ingredient-tags! i1 (cons ing-tag tags1))
	(set-ingredient-tags! i2 (cons ing-tag tags2)))))

(define scallions (make-ingredient "scallions" '()))
(define green-onion (make-ingredient "green onion" '()))
(set-ingredient=! scallions green-onion)
(ingredient= scallions green-onion)

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



