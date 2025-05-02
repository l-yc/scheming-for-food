(load "utils.scm")

;; define quantity

(define-record-type <quantity>
  (%make-quantity amount unit)
  quantity?
  (amount quantity-amount)
  (unit quantity-unit))

(define (make-quantity amount unit)
  (assert (unit? unit) "make-quantity: received invalid unit" unit)
  (%make-quantity amount unit))

(define-print-method
  quantity?
  (standard-print-method
   "quantity"
   (lambda (q)
     `(("amount" ,(quantity-amount q))
       ("unit" ,(quantity-unit q))))))

;; Units are represented as symbols
(define (unit? thing)
  (or (member '(tsp tbsp floz cup pt qt gal ml l) thing) ;; Volume
      (member '(oz lb g kg) thing)                       ;; Weight
      (member '(F C) thing)                              ;; Temperature

      (member '(pinch jar taste clove) thing)))          ;; Misc???

;; Given a unit, returns the canonical unit that should be used in place of it.
(define (canonical-unit unit)
  (case unit
    ((tsp tbsp floz cup pt qt gal ml l) 'ml)
    ((oz lb g kg) 'g)
    ((F C) 'C)
    (else (error "canonical-unit: unknown unit" unit))))

;; Given a unit, returns a procedure that can be used to take a amount in
;; that unit and transform it into an amount in the canonical unit for the
;; given unit.
;; Example: given tsp, returns a procedure that multiplies the input by
;; 4.928922, as this is the conversion ratio from tsp -> mL, the caconical unit
;; for volume.
(define (conversion-to-canonical unit)
  (define (times-by factor)
    (lambda (x) (* x factor)))
  (case unit
    ;; Volume: canonical unit mL
    ((tsp)  (times-by 4.928922))
    ((tbsp) (times-by 14.78677))
    ((floz) (times-by 29.57353))
    ((cup)  (times-by 236.5882))
    ((pt)   (times-by 473.1765))
    ((qt)   (times-by 946.353))
    ((gal)  (times-by 3785.412))
    ((ml)   (times-by 1.))
    ((l)    (times-by 1000.))
    ;; Weight: canonical unit g
    ((oz)   (times-by 28.34952))
    ((lb)   (times-by 453.5924))
    ((g)    (times-by 1.))
    ((kg)   (times-by 1000.))
    ;; Tempererature: canonical unit C
    ((F)    (lambda (f) (/ (* 5. (- f 32.)) 9.)))
    ((C)    (times-by 1.))
    (else   (error "conversion-to-canonical: unknown unit" unit))))

;; Converts a quantity into an equivalent quantity in canonical units
;; TODO: should this automatically be called in make-quantity?
(define (canonicalize-quantity q)
  (let ((unit (quantity-unit q)) (amount (quantity-amount q)))
    (make-quantity
      ((conversion-to-canonical unit) amount)
      (canonical-unit unit))))
