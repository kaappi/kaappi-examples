;;; Metacircular Scheme Evaluator
;;;
;;; A self-interpreting Scheme interpreter built entirely from the
;;; host compiler's primitives. Implements lexical scoping, closures,
;;; mutation, and the classic eval/apply loop.
;;;
;;; Supports: quote, if, cond, lambda, define, set!, begin, let,
;;; let*, and, or — plus 35+ built-in primitives.
;;;
;;; Usage:
;;;   kaappi app.scm demo
;;;   kaappi app.scm eval "(+ 1 2)"
;;;   kaappi app.scm eval "(define (f x) (* x x)) (f 5)"
;;;   kaappi app.scm repl
;;;
;;; Prerequisites: none (pure Scheme)

(import (scheme base) (scheme write) (scheme read)
        (scheme process-context) (scheme cxr))

;; --- Environment ---
;; Ribcage model: env = (frame ...), frame = ((var . val) ...)

(define (make-frame vars vals)
  (map cons vars vals))

(define (extend-env params args env)
  (cons (bind-params params args) env))

(define (bind-params params args)
  (cond
    ((null? params)
     (if (null? args) '()
         (error "Too many arguments")))
    ((symbol? params)
     (list (cons params args)))
    ((pair? params)
     (if (null? args)
         (error "Too few arguments")
         (cons (cons (car params) (car args))
               (bind-params (cdr params) (cdr args)))))
    (else (error "Invalid parameter list"))))

(define (lookup-variable var env)
  (if (null? env)
      (error "Unbound variable" var)
      (let ((binding (assq var (car env))))
        (if binding
            (cdr binding)
            (lookup-variable var (cdr env))))))

(define (set-variable! var val env)
  (if (null? env)
      (error "Unbound variable — set!" var)
      (let ((binding (assq var (car env))))
        (if binding
            (set-cdr! binding val)
            (set-variable! var val (cdr env))))))

(define (define-variable! var val env)
  (let ((binding (assq var (car env))))
    (if binding
        (set-cdr! binding val)
        (set-car! env (cons (cons var val) (car env))))))

;; --- Expression Analysis ---

(define (self-evaluating? e)
  (or (number? e) (boolean? e) (string? e) (char? e)))

(define (tagged-list? e tag)
  (and (pair? e) (eq? (car e) tag)))

(define (quoted? e)     (tagged-list? e 'quote))
(define (definition? e) (tagged-list? e 'define))
(define (assignment? e) (tagged-list? e 'set!))
(define (if? e)         (tagged-list? e 'if))
(define (lambda? e)     (tagged-list? e 'lambda))
(define (begin? e)      (tagged-list? e 'begin))
(define (cond? e)       (tagged-list? e 'cond))
(define (let? e)        (tagged-list? e 'let))
(define (let*? e)       (tagged-list? e 'let*))
(define (and? e)        (tagged-list? e 'and))
(define (or? e)         (tagged-list? e 'or))

(define (closure? e)      (tagged-list? e 'closure))
(define (closure-params c) (cadr c))
(define (closure-body c)   (caddr c))
(define (closure-env c)    (cadddr c))

;; --- Evaluator ---

(define (meta-eval expr env)
  (cond
    ((self-evaluating? expr) expr)
    ((symbol? expr) (lookup-variable expr env))
    ((quoted? expr) (cadr expr))

    ((definition? expr)
     (if (pair? (cadr expr))
         ;; (define (f x) body...) shorthand
         (let ((name (car (cadr expr)))
               (params (cdr (cadr expr)))
               (body (cddr expr)))
           (define-variable! name (list 'closure params body env) env))
         ;; (define x val)
         (define-variable! (cadr expr) (meta-eval (caddr expr) env) env)))

    ((assignment? expr)
     (set-variable! (cadr expr) (meta-eval (caddr expr) env) env))

    ((if? expr)
     (let ((test (meta-eval (cadr expr) env)))
       (if test
           (meta-eval (caddr expr) env)
           (if (pair? (cdddr expr))
               (meta-eval (cadddr expr) env)))))

    ((cond? expr) (eval-cond (cdr expr) env))

    ((lambda? expr)
     (list 'closure (cadr expr) (cddr expr) env))

    ((begin? expr) (eval-sequence (cdr expr) env))

    ((let? expr)
     (let* ((bindings (cadr expr))
            (body (cddr expr))
            (vars (map car bindings))
            (vals (map (lambda (b) (meta-eval (cadr b) env)) bindings))
            (new-env (cons (make-frame vars vals) env)))
       (eval-sequence body new-env)))

    ((let*? expr) (eval-let* (cadr expr) (cddr expr) env))
    ((and? expr) (eval-and (cdr expr) env))
    ((or? expr)  (eval-or (cdr expr) env))

    ((pair? expr)
     (let ((proc (meta-eval (car expr) env))
           (args (eval-list (cdr expr) env)))
       (meta-apply proc args)))

    (else (error "Unknown expression type" expr))))

(define (eval-sequence exprs env)
  (cond
    ((null? exprs) (if #f #f))
    ((null? (cdr exprs)) (meta-eval (car exprs) env))
    (else
     (meta-eval (car exprs) env)
     (eval-sequence (cdr exprs) env))))

(define (eval-list exprs env)
  (if (null? exprs) '()
      (cons (meta-eval (car exprs) env)
            (eval-list (cdr exprs) env))))

(define (eval-cond clauses env)
  (cond
    ((null? clauses) (if #f #f))
    ((eq? (caar clauses) 'else)
     (eval-sequence (cdar clauses) env))
    ((meta-eval (caar clauses) env)
     (eval-sequence (cdar clauses) env))
    (else (eval-cond (cdr clauses) env))))

(define (eval-let* bindings body env)
  (if (null? bindings)
      (eval-sequence body env)
      (let* ((b (car bindings))
             (val (meta-eval (cadr b) env))
             (new-env (cons (list (cons (car b) val)) env)))
        (eval-let* (cdr bindings) body new-env))))

(define (eval-and exprs env)
  (cond
    ((null? exprs) #t)
    ((null? (cdr exprs)) (meta-eval (car exprs) env))
    (else
     (let ((val (meta-eval (car exprs) env)))
       (if val (eval-and (cdr exprs) env) #f)))))

(define (eval-or exprs env)
  (cond
    ((null? exprs) #f)
    ((null? (cdr exprs)) (meta-eval (car exprs) env))
    (else
     (let ((val (meta-eval (car exprs) env)))
       (if val val (eval-or (cdr exprs) env))))))

;; --- Application ---

(define (meta-apply proc args)
  (cond
    ((closure? proc)
     (eval-sequence (closure-body proc)
                    (extend-env (closure-params proc) args
                                (closure-env proc))))
    ((procedure? proc)
     (apply proc args))
    (else (error "Not a procedure" proc))))

;; --- Primitive Bridges ---

(define (meta-map proc lst)
  (if (null? lst) '()
      (cons (meta-apply proc (list (car lst)))
            (meta-map proc (cdr lst)))))

(define (meta-for-each proc lst)
  (if (null? lst) (if #f #f)
      (begin (meta-apply proc (list (car lst)))
             (meta-for-each proc (cdr lst)))))

(define (meta-apply-prim proc args)
  (meta-apply proc args))

;; --- Global Environment ---

(define (make-global-env)
  (list
   (list
    (cons '+ +) (cons '- -) (cons '* *) (cons '/ /)
    (cons '= =) (cons '< <) (cons '> >) (cons '<= <=) (cons '>= >=)
    (cons 'zero? zero?) (cons 'abs abs)
    (cons 'remainder remainder) (cons 'modulo modulo)
    (cons 'min min) (cons 'max max) (cons 'expt expt)
    (cons 'cons cons) (cons 'car car) (cons 'cdr cdr)
    (cons 'list list) (cons 'null? null?) (cons 'pair? pair?)
    (cons 'length length) (cons 'append append)
    (cons 'cadr cadr) (cons 'caddr caddr)
    (cons 'number? number?) (cons 'symbol? symbol?)
    (cons 'boolean? boolean?) (cons 'string? string?)
    (cons 'eq? eq?) (cons 'equal? equal?) (cons 'not not)
    (cons 'display display) (cons 'newline newline) (cons 'write write)
    (cons 'map meta-map) (cons 'for-each meta-for-each)
    (cons 'apply meta-apply-prim)
    (cons 'error error))))

;; --- Program Runner ---

(define (run-program program-str)
  (let ((env (make-global-env))
        (port (open-input-string program-str)))
    (let loop ((result (if #f #f)))
      (let ((expr (read port)))
        (if (eof-object? expr)
            result
            (loop (meta-eval expr env)))))))

;; --- Demo ---

(define demo-factorial
"(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
(fact 10)")

(define demo-fibonacci
"(define (fib n)
  (cond ((= n 0) 0) ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))
(fib 10)")

(define demo-higher-order
"(define (my-filter p lst)
  (cond ((null? lst) '())
        ((p (car lst))
         (cons (car lst) (my-filter p (cdr lst))))
        (else (my-filter p (cdr lst)))))
(my-filter (lambda (x) (> x 10))
           (map (lambda (x) (* x x)) (list 1 2 3 4 5)))")

(define demo-closures
"(define (make-counter)
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      n)))
(define c (make-counter))
(list (c) (c) (c))")

(define demo-mutual-recursion
"(define (my-even? n)
  (if (= n 0) #t (my-odd? (- n 1))))
(define (my-odd? n)
  (if (= n 0) #f (my-even? (- n 1))))
(list (my-even? 10) (my-odd? 11) (my-even? 7))")

(define (run-demo)
  (define (show label program)
    (display "  ") (display label) (newline)
    (let ((env (make-global-env))
          (port (open-input-string program)))
      (let loop ((result (if #f #f)))
        (let ((expr (read port)))
          (if (eof-object? expr)
              (begin
                (display "  => ") (write result) (newline) (newline))
              (begin
                (display "  > ") (write expr) (newline)
                (loop (meta-eval expr env))))))))
  (display "--- Metacircular Evaluator Demo ---") (newline)
  (display "--- Scheme interpreting Scheme ---") (newline)
  (newline)
  (show "Factorial — recursive:" demo-factorial)
  (show "Fibonacci — cond & double recursion:" demo-fibonacci)
  (show "Higher-order — map & filter:" demo-higher-order)
  (show "Closures — mutable state:" demo-closures)
  (show "Mutual recursion — even?/odd?:" demo-mutual-recursion))

;; --- Main ---

(let ((args (command-line)))
  (if (< (length args) 3)
      (begin
        (display "Usage: kaappi app.scm <command> [args...]") (newline)
        (display "Commands:") (newline)
        (display "  demo             Run example programs in the interpreter") (newline)
        (display "  eval PROGRAM     Evaluate a program string") (newline)
        (display "  repl             Interactive meta-Scheme REPL") (newline))
      (let ((cmd (list-ref args 2)))
        (cond
          ((equal? cmd "demo")
           (run-demo))

          ((equal? cmd "eval")
           (if (< (length args) 4)
               (begin
                 (display "Usage: kaappi app.scm eval PROGRAM") (newline)
                 (display "Example: kaappi app.scm eval \"(+ 1 2)\"") (newline))
               (let ((result (run-program (list-ref args 3))))
                 (write result) (newline))))

          ((equal? cmd "repl")
           (let ((env (make-global-env)))
             (display "Metacircular Scheme REPL") (newline)
             (display "Type expressions (one per line). Ctrl-D to exit.") (newline)
             (let loop ()
               (display "meta> ") (flush-output-port)
               (let ((line (read-line)))
                 (cond
                   ((eof-object? line) (newline))
                   ((zero? (string-length line)) (loop))
                   (else
                    (guard (e (#t (display "Error: ")
                                  (if (error-object? e)
                                      (display (error-object-message e))
                                      (write e))
                                  (newline) (flush-output-port)))
                      (let* ((expr (read (open-input-string line)))
                             (result (meta-eval expr env)))
                        (write result) (newline) (flush-output-port)))
                    (loop)))))))

          (else
           (display "Unknown command: ") (display cmd) (newline)
           (display "Commands: demo, eval, repl") (newline))))))
