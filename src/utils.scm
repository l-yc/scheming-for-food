;; debug
(define print-dbg #t)

(define (expect a b)
  (if (not (equal? a b)) (error "expected eqv?" a b ) 'ok))

(define (dbg tag . args)
  (if print-dbg
      (pp (list 'debug: tag args))))


;; list
;; Check if x is a member of l
(define (in? l x)
  (if (memv x l) #t #f))


(define (unique-sorted sorted-lst)
  (let loop ((lst sorted-lst)
	     (prv ""))
					;(dbg 'loop (car lst) prv)
    (cond ((null? lst) '())
	  ((string=? prv (car lst)) (loop (cdr lst) prv))
	  (else (cons (car lst) (loop (cdr lst) (car lst)))))))
