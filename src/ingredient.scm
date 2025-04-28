(load "utils.scm")

;; define ingredient object
(define-record-type <ingredient>
  (make-ingredient canonical-name names tags)
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

;; tests
(define chicken (make-simple-ingredient "chicken"))
(display chicken)



;; define ingredients relations

