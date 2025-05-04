;(load "./sdf/manager/load")
;(manage 'new 'generic-procedures)


;; REPL

;;; Global environment for REPL.
(define the-global-environment
  'not-initialized)

(define the-empty-environment (list '*the-empty-environment*))

(define initial-env-bindings '())

(define (extend-environment variables values base-environment)
  (let lp ((invars variables) (invals values)
           (outvars '())      (outvals '()))
    (cond ((null? invars)
           (if (null? invals)
               (vector outvars outvals base-environment)
               (error "Too many arguments supplied"
                      variables values)))
          ((symbol? invars)
           (vector (cons invars outvars)
                   (cons invals outvals)
                   base-environment))
          ((pair? invars)
           (if (pair? invals)
               (lp (cdr invars)
                   (cdr invals)
                   (cons (car invars) outvars)
                   (cons (car invals) outvals))
               (error "Too few arguments supplied"
                      variables values)))
          (else
           (error "Bad formal parameter list"
                  variables values)))))

(define (make-global-environment)
  (extend-environment (map car initial-env-bindings)
                      (map cdr initial-env-bindings)
                      the-empty-environment))

(define (initialize-repl!)
  (set! the-global-environment (make-global-environment))
  'done)

(define (check-repl-initialized)
  (if (eq? the-global-environment 'not-initialized)
      (error "Interpreter not initialized. Run (init) first.")))

(define (g:read)
  (prompt-for-command-expression "sff> "))

(define (init)
  (initialize-repl!)
  (repl))

(define (go)
  (repl))








(define (repl)
  (check-repl-initialized)
  (let ((input (g:read)))
    (write-line (q:eval input the-global-environment))
    (repl)))


;; handlers

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))
;(register-predicate! tagged-list? 'tagged-list)

(define (ingredients-query? exp)
  (tagged-list? exp 'ingredients))

(define (list-query? exp)
  (tagged-list? exp 'list))

;; ingredients
(define (get-var var env) (cdr (assv var env)))

(define (eval-ingredients-query expression environment)
  (cond ((list-query? expression)
	 (get-var '%ingredients environment))
	(else 'invalid-ingredients-query)))
  
(define (q:eval expression environment)
  (cond ((ingredients-query? expression)
	 (eval-ingredients-query (cdr expression) environment))
	(else 'invalid-query)))

	 

(init)




;; too complicated, i think i'll just bash this
;(define q:eval
;  (simple-generic-procedure 'q:eval 2 default-eval))
;
;(define (environment? x) #t)
;
;(environment? the-global-environment)
;
;(define (eval-ingredients-query expression environment)
;  
;  'ingredients)
;
;(define-generic-procedure-handler q:eval
;  (match-args ingredients-query? environment?)
;  eval-ingredients-query)
