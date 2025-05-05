;(load "./sdf/manager/load")
;(manage 'new 'generic-procedures)
(load "data_loader.scm")


;; REPL

;; from SDF
(define (environment? x) #t)     ;make better!
(register-predicate! environment? 'environment)

(define (extend-environment variables values base-environment)
  (if (not (fix:= (length variables) (length values)))
      (if (fix:< (length variables) (length values))
          (error "Too many arguments supplied" variables values)
          (error "Too few arguments supplied" variables values)))
  (vector variables values base-environment))

(define (environment-variables env) (vector-ref env 0))
(define (environment-values env) (vector-ref env 1))
(define (environment-parent env) (vector-ref env 2))

(define the-empty-environment (list '*the-empty-environment*))

(define (lookup-variable-value var env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
        (lookup-scheme-value var)
        (let scan
            ((vars (vector-ref env 0))
             (vals (vector-ref env 1)))
          (cond ((null? vars) (plp (vector-ref env 2)))
                ((eq? var (car vars)) (car vals))
                (else (scan (cdr vars) (cdr vals))))))))

(define lookup-scheme-value
  (let ((env (the-environment)))
    (named-lambda (lookup-scheme-value var)
      (lexical-reference env var))))

(define (define-variable! var val env)
  (let scan
      ((vars (vector-ref env 0))
       (vals (vector-ref env 1)))
    (cond ((null? vars)
           (vector-set! env 0 (cons var (vector-ref env 0)))
           (vector-set! env 1 (cons val (vector-ref env 1))))
          ((eq? var (car vars))
           (set-car! vals val))
          (else
           (scan (cdr vars) (cdr vals))))))

(define (set-variable-value! var val env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var))
    (let scan
        ((vars (vector-ref env 0))
         (vals (vector-ref env 1)))
      (cond ((null? vars) (plp (vector-ref env 2)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))))

(define initial-env-bindings '())

(define (define-initial-env-binding name value)
  (let ((p (assq name initial-env-bindings)))
    (if p
        (set-cdr! p value)
        (set! initial-env-bindings
              (cons (cons name value) initial-env-bindings))))
  name)

(define (make-global-environment)
  (extend-environment (map car initial-env-bindings)
                      (map cdr initial-env-bindings)
                      the-empty-environment))

(define the-global-environment
  'not-initialized)

(define (initialize-repl!)
  (define-initial-env-binding '%recipes '())
  (define-initial-env-binding '%ingredients '())
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

(define (ingredients-query? exp)
  (tagged-list? exp 'ingredients))

(define (recipes-query? exp)
  (tagged-list? exp 'ingredients))

(define (list-query? exp)
  (tagged-list? exp 'list))

(define (load-query? exp)
  (tagged-list? exp 'load))



;; helpers
(define (read-to-list-by-line filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '())
                 (next-line (read-line)))
	(if (eof-object? next-line)
	    (reverse lines)         
	    (let ((thing (read (open-input-string next-line))))
	      (loop (cons thing lines)
		    (read-line))))))))

(define (read-to-list filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '())
                 (next-line (read)))
	(if (eof-object? next-line)
	    (reverse lines)         
	    (loop (cons next-line lines)
		  (read-line)))))))



;; ingredients
(define (list-ingredients expression environment)
  (let ((ingredients (lookup-variable-value '%ingredients environment)))
    (let scan ((i ingredients))
      (if (pair? i)
	  (begin
	    (write-line (ingredient-name (car i)))
	    (scan (cdr i)))))
    (list (length ingredients) 'ingredients)))
  
(define (load-ingredients expression environment)
  (let ((filename (cadr expression)))
    (let ((lines (read-to-list-by-line filename)))
      (let ((ingredients (map (lambda (item)
				(let ((name (car item))
				      (tags (cdr item)))
				  (make-ingredient name tags))) lines)))
	(set-variable-value! '%ingredients
			     ingredients
			     environment)))))
  

(define (eval-ingredients-query expression environment)
  (cond ((list-query? expression)
	 (list-ingredients expression environment))
	((load-query? expression)
	 (load-ingredients expression environment))
	(else 'invalid-ingredients-query)))

(define (list-ingredients expression environment)
  (lookup-variable-value '%recipes environment))

(define (eval-recipes-query expression environment)
  (cond ((list-query? expression)
	 (list-ingredients expression environment))
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
