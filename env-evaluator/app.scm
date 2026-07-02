;;; Environment-Model Evaluator (Eager & Lazy)
;;;
;;; A Scheme evaluator with explicit environment chains and switchable
;;; evaluation strategy — applicative-order (eager) and normal-order
;;; (lazy) with memoizing thunks.
;;;
;;; Key differences from the basic metacircular evaluator:
;;;   - Lazy evaluation: arguments wrapped as thunks, forced on demand
;;;   - Call-by-need: thunks memoized after first force
;;;   - letrec for proper mutual recursion
;;;   - Evaluation strategy comparison demos
;;;
;;; Usage:
;;;   kaappi app.scm demo
;;;   kaappi app.scm eval "(+ 1 2)"
;;;   kaappi app.scm lazy "(define (try a b) (if (= a 0) 1 b)) (try 0 (/ 1 0))"
;;;   kaappi app.scm repl
;;;
;;; Prerequisites: none (pure Scheme)

(import (scheme base) (scheme write) (scheme read)
        (scheme process-context) (scheme cxr))

;; --- Evaluation Strategy ---

(define *lazy* #f)

;; --- Environment Model ---
;; Ribcage: env = (frame ...), frame = ((var . val) ...)
;; Lookup walks innermost frame to outermost. In lazy mode,
;; variable values may be thunks — forced transparently on lookup.

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

(define (lookup-var var env)
  (if (null? env)
      (error "Unbound variable" var)
      (let ((binding (assq var (car env))))
        (if binding
            (force-value (cdr binding))
            (lookup-var var (cdr env))))))

(define (set-var! var val env)
  (if (null? env)
      (error "Unbound variable — set!" var)
      (let ((binding (assq var (car env))))
        (if binding
            (set-cdr! binding val)
            (set-var! var val (cdr env))))))

(define (define-var! var val env)
  (let ((binding (assq var (car env))))
    (if binding
        (set-cdr! binding val)
        (set-car! env (cons (cons var val) (car env))))))

;; --- Thunks ---
;; In lazy mode, application arguments are wrapped as (thunk expr env)
;; instead of being evaluated. When a variable bound to a thunk is
;; looked up, force-value evaluates the thunk and memoizes the result
;; by mutating it in-place to (forced result). This gives call-by-need
;; semantics: each thunk is evaluated at most once.

(define (make-thunk expr env) (list 'thunk expr env))
(define (thunk? v) (and (pair? v) (eq? (car v) 'thunk)))
(define (forced? v) (and (pair? v) (eq? (car v) 'forced)))

(define (force-value v)
  (cond
    ((thunk? v)
     (let ((result (mc-eval (cadr v) (caddr v))))
       (set-car! v 'forced)
       (set-cdr! v (list result))
       result))
    ((forced? v) (cadr v))
    (else v)))

(define (force-args args)
  (if (null? args) '()
      (cons (force-value (car args))
            (force-args (cdr args)))))

;; --- Expression Predicates ---

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
(define (letrec? e)     (tagged-list? e 'letrec))
(define (and? e)        (tagged-list? e 'and))
(define (or? e)         (tagged-list? e 'or))

(define (closure? e)       (tagged-list? e 'closure))
(define (closure-params c) (cadr c))
(define (closure-body c)   (caddr c))
(define (closure-env c)    (cadddr c))

;; --- Core Evaluator ---

(define (mc-eval expr env)
  (cond
    ((self-evaluating? expr) expr)
    ((symbol? expr) (lookup-var expr env))
    ((quoted? expr) (cadr expr))

    ((definition? expr)
     (if (pair? (cadr expr))
         (let ((name (car (cadr expr)))
               (params (cdr (cadr expr)))
               (body (cddr expr)))
           (define-var! name (list 'closure params body env) env))
         (define-var! (cadr expr) (mc-eval (caddr expr) env) env)))

    ((assignment? expr)
     (set-var! (cadr expr) (mc-eval (caddr expr) env) env))

    ((if? expr)
     (let ((test (mc-eval (cadr expr) env)))
       (if test
           (mc-eval (caddr expr) env)
           (if (pair? (cdddr expr))
               (mc-eval (cadddr expr) env)))))

    ((cond? expr) (eval-cond (cdr expr) env))

    ((lambda? expr)
     (list 'closure (cadr expr) (cddr expr) env))

    ((begin? expr) (eval-sequence (cdr expr) env))

    ((let? expr)
     (let* ((bindings (cadr expr))
            (body (cddr expr))
            (vars (map car bindings))
            (vals (map (lambda (b) (mc-eval (cadr b) env)) bindings))
            (new-env (cons (make-frame vars vals) env)))
       (eval-sequence body new-env)))

    ((let*? expr) (eval-let* (cadr expr) (cddr expr) env))

    ((letrec? expr)
     (let* ((bindings (cadr expr))
            (body (cddr expr))
            (vars (map car bindings))
            (temps (map (lambda (_) (if #f #f)) vars))
            (new-env (cons (make-frame vars temps) env)))
       (for-each
        (lambda (b)
          (set-var! (car b) (mc-eval (cadr b) new-env) new-env))
        bindings)
       (eval-sequence body new-env)))

    ((and? expr) (eval-and (cdr expr) env))
    ((or? expr)  (eval-or (cdr expr) env))

    ;; Application — the key branch point for lazy evaluation.
    ;; Eager: evaluate all arguments before applying.
    ;; Lazy: wrap arguments as thunks; they are forced on demand
    ;; when the callee's body references its parameters.
    ((pair? expr)
     (let ((proc (mc-eval (car expr) env))
           (args (if *lazy*
                     (map (lambda (a) (make-thunk a env)) (cdr expr))
                     (eval-list (cdr expr) env))))
       (mc-apply proc args)))

    (else (error "Unknown expression type" expr))))

(define (eval-sequence exprs env)
  (cond
    ((null? exprs) (if #f #f))
    ((null? (cdr exprs)) (mc-eval (car exprs) env))
    (else
     (mc-eval (car exprs) env)
     (eval-sequence (cdr exprs) env))))

(define (eval-list exprs env)
  (if (null? exprs) '()
      (cons (mc-eval (car exprs) env)
            (eval-list (cdr exprs) env))))

(define (eval-cond clauses env)
  (cond
    ((null? clauses) (if #f #f))
    ((eq? (caar clauses) 'else)
     (eval-sequence (cdar clauses) env))
    ((mc-eval (caar clauses) env)
     (eval-sequence (cdar clauses) env))
    (else (eval-cond (cdr clauses) env))))

(define (eval-let* bindings body env)
  (if (null? bindings)
      (eval-sequence body env)
      (let* ((b (car bindings))
             (val (mc-eval (cadr b) env))
             (new-env (cons (list (cons (car b) val)) env)))
        (eval-let* (cdr bindings) body new-env))))

(define (eval-and exprs env)
  (cond
    ((null? exprs) #t)
    ((null? (cdr exprs)) (mc-eval (car exprs) env))
    (else
     (let ((val (mc-eval (car exprs) env)))
       (if val (eval-and (cdr exprs) env) #f)))))

(define (eval-or exprs env)
  (cond
    ((null? exprs) #f)
    ((null? (cdr exprs)) (mc-eval (car exprs) env))
    (else
     (let ((val (mc-eval (car exprs) env)))
       (if val val (eval-or (cdr exprs) env))))))

;; --- Application ---
;; Closures: args may be thunks (forced when body looks up params).
;; Host primitives: must force all args to concrete values first.

(define (mc-apply proc args)
  (cond
    ((closure? proc)
     (eval-sequence (closure-body proc)
                    (extend-env (closure-params proc) args
                                (closure-env proc))))
    ((procedure? proc)
     (apply proc (force-args args)))
    (else (error "Not a procedure" proc))))

;; --- Primitive Bridges ---

(define (meta-map proc lst)
  (if (null? lst) '()
      (cons (mc-apply proc (list (car lst)))
            (meta-map proc (cdr lst)))))

(define (meta-for-each proc lst)
  (if (null? lst) (if #f #f)
      (begin (mc-apply proc (list (car lst)))
             (meta-for-each proc (cdr lst)))))

(define (meta-apply-prim proc args)
  (mc-apply proc args))

;; --- Global Environment ---

(define (make-global-env)
  (list
   (list
    (cons '+ +) (cons '- -) (cons '* *) (cons '/ /)
    (cons '= =) (cons '< <) (cons '> >) (cons '<= <=) (cons '>= >=)
    (cons 'zero? zero?) (cons 'abs abs)
    (cons 'remainder remainder) (cons 'modulo modulo)
    (cons 'min min) (cons 'max max) (cons 'expt expt)
    (cons 'number->string number->string)
    (cons 'cons cons) (cons 'car car) (cons 'cdr cdr)
    (cons 'list list) (cons 'null? null?) (cons 'pair? pair?)
    (cons 'length length) (cons 'append append) (cons 'reverse reverse)
    (cons 'cadr cadr) (cons 'caddr caddr)
    (cons 'number? number?) (cons 'symbol? symbol?)
    (cons 'boolean? boolean?) (cons 'string? string?)
    (cons 'procedure? (lambda (x) (or (procedure? x) (closure? x))))
    (cons 'eq? eq?) (cons 'equal? equal?) (cons 'not not)
    (cons 'display display) (cons 'newline newline) (cons 'write write)
    (cons 'map meta-map) (cons 'for-each meta-for-each)
    (cons 'apply meta-apply-prim)
    (cons 'error error))))

;; --- Program Runner ---

(define (run-program program-str)
  (set! *lazy* #f)
  (let ((env (make-global-env))
        (port (open-input-string program-str)))
    (let loop ((result (if #f #f)))
      (let ((expr (read port)))
        (if (eof-object? expr)
            result
            (loop (mc-eval expr env)))))))

(define (run-program-lazy program-str)
  (set! *lazy* #t)
  (let ((env (make-global-env))
        (port (open-input-string program-str)))
    (let loop ((result (if #f #f)))
      (let ((expr (read port)))
        (if (eof-object? expr)
            (let ((final (force-value result)))
              (set! *lazy* #f)
              final)
            (loop (mc-eval expr env)))))))

;; --- Demo Programs ---

(define demo-scopes
"(let ((x 10))
  (let ((x 20) (y x))
    (list x y)))")

(define demo-closures
"(define (make-adder n) (lambda (x) (+ n x)))
(define add5 (make-adder 5))
(define add10 (make-adder 10))
(list (add5 3) (add10 3) (add5 (add10 1)))")

(define demo-mutation
"(define (make-counter)
  (let ((n 0))
    (lambda (msg)
      (cond ((eq? msg 'inc) (set! n (+ n 1)) n)
            ((eq? msg 'dec) (set! n (- n 1)) n)
            ((eq? msg 'val) n)))))
(define c (make-counter))
(c 'inc) (c 'inc) (c 'inc) (c 'dec)
(c 'val)")

(define demo-letrec
"(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
         (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))))
  (list (even? 10) (odd? 11) (even? 7)))")

(define demo-higher-order
"(define (my-map f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (my-map f (cdr lst)))))
(define (my-filter p lst)
  (cond ((null? lst) '())
        ((p (car lst)) (cons (car lst) (my-filter p (cdr lst))))
        (else (my-filter p (cdr lst)))))
(my-filter (lambda (x) (> x 10))
           (my-map (lambda (x) (* x x))
                   (list 1 2 3 4 5 6)))")

(define demo-lazy-safe
"(define (try a b) (if (= a 0) 1 b))
(try 0 (/ 1 0))")

(define demo-lazy-diverge
"(define (my-if pred then-val else-val)
  (if pred then-val else-val))
(define (diverge) (diverge))
(my-if #t 42 (diverge))")

(define demo-lazy-memo
"(define (side-effect)
  (display \"  [thunk forced]\") (newline)
  42)
(define (square x) (* x x))
(square (side-effect))")

;; --- Demo Runner ---

(define (show label program)
  (display "  ") (display label) (newline)
  (set! *lazy* #f)
  (let ((env (make-global-env))
        (port (open-input-string program)))
    (let loop ((result (if #f #f)))
      (let ((expr (read port)))
        (if (eof-object? expr)
            (begin (display "  => ") (write result) (newline) (newline))
            (begin (display "  > ") (write expr) (newline)
                   (loop (mc-eval expr env))))))))

(define (show-lazy label program)
  (display "  ") (display label) (newline)
  (set! *lazy* #t)
  (let ((env (make-global-env))
        (port (open-input-string program)))
    (let loop ((result (if #f #f)))
      (let ((expr (read port)))
        (if (eof-object? expr)
            (let ((final (force-value result)))
              (set! *lazy* #f)
              (display "  => ") (write final) (newline) (newline))
            (begin (display "  > ") (write expr) (newline)
                   (loop (mc-eval expr env))))))))

(define (run-demo)
  (display "--- Environment-Model Evaluator Demo ---") (newline)
  (display "--- Eager & Lazy Evaluation Strategies ---") (newline)
  (newline)

  (display "  === EAGER (Applicative-Order) ===") (newline) (newline)

  (show "1. Nested scopes (inner x shadows, y captures outer):"
        demo-scopes)

  (show "2. Closures (factory captures definition environment):"
        demo-closures)

  (show "3. Mutable state (counter via set! inside closure):"
        demo-mutation)

  (show "4. Letrec (mutual recursion: even?/odd?):"
        demo-letrec)

  (show "5. Higher-order (map & filter in hosted language):"
        demo-higher-order)

  (display "  === LAZY (Normal-Order) ===") (newline) (newline)

  (display "  6. Lazy avoids error — (/ 1 0) never forced:") (newline)
  (display "  Eager: ")
  (guard (e (#t (display "error — ")
               (if (error-object? e)
                   (display (error-object-message e))
                   (write e))
               (newline)))
    (let ((r (run-program demo-lazy-safe)))
      (display "ok => ") (write r) (newline)))
  (show-lazy "Lazy:" demo-lazy-safe)

  (display "  7. Lazy avoids divergence — my-if as procedure:") (newline)
  (display "  Eager: would loop forever (all args evaluated before apply)")
  (newline)
  (show-lazy "Lazy:" demo-lazy-diverge)

  (display "  8. Thunk memoization — side effect fires once:") (newline)
  (display "  (square calls (* x x), x's thunk forced only once)")
  (newline)
  (show-lazy "Lazy:" demo-lazy-memo)
  (display "  (\"[thunk forced]\" printed once, not twice — call-by-need)")
  (newline))

;; --- Main ---

(define (user-args)
  (let ((args (command-line)))
    (cond
      ((null? args) '())
      ((let ((s (car args)))
         (and (>= (string-length s) 4)
              (equal? (substring s (- (string-length s) 4) (string-length s))
                      ".scm")))
       (cdr args))
      ((and (pair? (cdr args))
            (let ((s (cadr args)))
              (and (>= (string-length s) 4)
                   (equal? (substring s (- (string-length s) 4)
                                        (string-length s))
                           ".scm"))))
       (cddr args))
      (else (cdr args)))))

(let ((args (user-args)))
  (if (null? args)
      (begin
        (display "Usage: kaappi app.scm <command> [args...]") (newline)
        (display "Commands:") (newline)
        (display "  demo             Run demonstration programs") (newline)
        (display "  eval PROGRAM     Evaluate eagerly") (newline)
        (display "  lazy PROGRAM     Evaluate lazily (normal-order)") (newline)
        (display "  repl             Interactive REPL (eager)") (newline))
      (let ((cmd (car args)))
        (cond
          ((equal? cmd "demo")
           (run-demo))

          ((equal? cmd "eval")
           (if (< (length args) 2)
               (begin
                 (display "Usage: kaappi app.scm eval PROGRAM") (newline)
                 (display "Example: kaappi app.scm eval \"(+ 1 2)\"") (newline))
               (let ((result (run-program (cadr args))))
                 (write result) (newline))))

          ((equal? cmd "lazy")
           (if (< (length args) 2)
               (begin
                 (display "Usage: kaappi app.scm lazy PROGRAM") (newline)
                 (display "Example: kaappi app.scm lazy ")
                 (display "\"(define (try a b) (if (= a 0) 1 b)) (try 0 (/ 1 0))\"")
                 (newline))
               (let ((result (run-program-lazy (cadr args))))
                 (write result) (newline))))

          ((equal? cmd "repl")
           (let ((env (make-global-env)))
             (display "Environment-Model Scheme REPL (eager)") (newline)
             (display "Type expressions (one per line). Ctrl-D to exit.") (newline)
             (set! *lazy* #f)
             (let loop ()
               (display "eval> ") (flush-output-port)
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
                             (result (mc-eval expr env)))
                        (write result) (newline) (flush-output-port)))
                    (loop)))))))

          (else
           (display "Unknown command: ") (display cmd) (newline)
           (display "Commands: demo, eval, lazy, repl") (newline))))))
