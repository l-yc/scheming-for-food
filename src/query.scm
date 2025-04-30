(load "utils.scm")

;;; Global environment for REPL.
(define the-global-environment
  'not-initialized)

(define the-empty-environment (list '*the-empty-environment*))

(define (make-global-environment)
  the-empty-environment)
  ;(extend-environment (map car initial-env-bindings)
  ;                    (map cdr initial-env-bindings)
  ;                    the-empty-environment))

(define (initialize-repl!)
  (set! the-global-environment (make-global-environment))
  'done)

(define (check-repl-initialized)
  (if (eq? the-global-environment 'not-initialized)
      (error "Interpreter not initialized. Run (init) first.")))

(define (g:read)
  (prompt-for-command-expression "eval> "))

(define (init)
  (initialize-repl!)
  (repl))

(define (go)
  (repl))

(define (g:eval exp env)
  'todo)

(define (repl)
  (check-repl-initialized)
  (let ((input (g:read)))
    (write-line (g:eval input the-global-environment))
    (repl)))


(init)
(repl)
