(define print-dbg #t)

(define (expect a b)
  (if (not (equal? a b)) (error "expected eqv?" a b ) 'ok))

(define (dbg tag . args)
  (if print-dbg
      (pp (list 'debug: tag args))))

;; Removes duplicates from a sorted list of strings
(define (unique-sorted sorted-lst)
  (let loop ((lst sorted-lst)
             (prv ""))
    (cond ((null? lst) '())
     ((string=? prv (car lst)) (loop (cdr lst) prv))
     (else (cons (car lst) (loop (cdr lst) (car lst)))))))
