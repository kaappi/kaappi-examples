;;; Symbolic Differentiation
;;;
;;; Constructs abstract syntax trees for mathematical expressions
;;; and computes symbolic derivatives by recursive tree-walking.
;;;
;;; Supported operations: + - * / ^ sin cos exp ln
;;;
;;; Usage:
;;;   kaappi app.scm diff "(* x x)" x
;;;   kaappi app.scm diff "(sin (* 2 x))" x
;;;   kaappi app.scm simplify "(+ (* 1 x) (* y 0))"
;;;   kaappi app.scm eval "(+ (* 2 3) (- 10 4))"
;;;   kaappi app.scm demo
;;;
;;; Prerequisites: none (pure Scheme)

(import (scheme base) (scheme write) (scheme process-context)
        (scheme read) (scheme cxr) (scheme inexact))

;; --- AST Predicates & Accessors ---

(define (variable? e)    (symbol? e))
(define (sum? e)         (and (pair? e) (eq? (car e) '+)))
(define (product? e)     (and (pair? e) (eq? (car e) '*)))
(define (difference? e)  (and (pair? e) (eq? (car e) '-)))
(define (quotient-e? e)  (and (pair? e) (eq? (car e) '/)))
(define (power? e)       (and (pair? e) (eq? (car e) '^)))
(define (sin-expr? e)    (and (pair? e) (eq? (car e) 'sin)))
(define (cos-expr? e)    (and (pair? e) (eq? (car e) 'cos)))
(define (exp-expr? e)    (and (pair? e) (eq? (car e) 'exp)))
(define (ln-expr? e)     (and (pair? e) (eq? (car e) 'ln)))

(define lhs cadr)
(define rhs caddr)
(define operand cadr)

;; --- Simplification ---

(define (simplify e)
  (cond
    ((or (number? e) (symbol? e)) e)
    ((not (pair? e)) e)
    (else
     (let ((op (car e)))
       (if (memq op '(sin cos exp ln))
           (list op (simplify (operand e)))
           (let ((a (simplify (lhs e)))
                 (b (simplify (rhs e))))
             (cond
               ((and (number? a) (number? b))
                (case op
                  ((+) (+ a b)) ((-) (- a b)) ((*) (* a b))
                  ((/) (if (zero? b) (list op a b) (/ a b)))
                  ((^) (expt a b))
                  (else (list op a b))))
               ((and (eq? op '+) (eqv? a 0)) b)
               ((and (eq? op '+) (eqv? b 0)) a)
               ((and (eq? op '+) (equal? a b)) (list '* 2 a))
               ((and (eq? op '-) (eqv? b 0)) a)
               ((and (eq? op '-) (equal? a b)) 0)
               ((and (eq? op '*) (or (eqv? a 0) (eqv? b 0))) 0)
               ((and (eq? op '*) (eqv? a 1)) b)
               ((and (eq? op '*) (eqv? b 1)) a)
               ((and (eq? op '/) (eqv? a 0)) 0)
               ((and (eq? op '/) (eqv? b 1)) a)
               ((and (eq? op '/) (equal? a b)) 1)
               ((and (eq? op '^) (eqv? b 0)) 1)
               ((and (eq? op '^) (eqv? b 1)) a)
               ((and (eq? op '^) (eqv? a 1)) 1)
               (else (list op a b)))))))))

;; --- Differentiation ---

(define (differentiate expr var)
  (simplify
   (cond
     ;; d/dx(c) = 0
     ((number? expr) 0)

     ;; d/dx(x) = 1, d/dx(y) = 0
     ((variable? expr)
      (if (eq? expr var) 1 0))

     ;; d/dx(a + b) = da + db
     ((sum? expr)
      (list '+ (differentiate (lhs expr) var)
                (differentiate (rhs expr) var)))

     ;; d/dx(a - b) = da - db
     ((difference? expr)
      (list '- (differentiate (lhs expr) var)
                (differentiate (rhs expr) var)))

     ;; d/dx(a * b) = a*db + b*da  (product rule)
     ((product? expr)
      (let ((u (lhs expr)) (v (rhs expr)))
        (list '+ (list '* u (differentiate v var))
                  (list '* v (differentiate u var)))))

     ;; d/dx(a / b) = (b*da - a*db) / b^2  (quotient rule)
     ((quotient-e? expr)
      (let ((u (lhs expr)) (v (rhs expr)))
        (list '/ (list '- (list '* v (differentiate u var))
                           (list '* u (differentiate v var)))
                  (list '^ v 2))))

     ;; d/dx(a ^ n) = n * a^(n-1) * da  (power rule, n constant)
     ;; d/dx(a ^ b) = a^b * (db*ln(a) + b*da/a)  (general)
     ((power? expr)
      (let ((u (lhs expr)) (n (rhs expr)))
        (if (number? n)
            (list '* (list '* n (list '^ u (- n 1)))
                      (differentiate u var))
            (list '* (list '^ u n)
                      (list '+ (list '* (differentiate n var) (list 'ln u))
                                (list '/ (list '* n (differentiate u var))
                                          u))))))

     ;; d/dx(sin u) = cos(u) * du  (chain rule)
     ((sin-expr? expr)
      (let ((u (operand expr)))
        (list '* (list 'cos u) (differentiate u var))))

     ;; d/dx(cos u) = -sin(u) * du
     ((cos-expr? expr)
      (let ((u (operand expr)))
        (list '* (list '- 0 (list 'sin u)) (differentiate u var))))

     ;; d/dx(exp u) = exp(u) * du
     ((exp-expr? expr)
      (let ((u (operand expr)))
        (list '* (list 'exp u) (differentiate u var))))

     ;; d/dx(ln u) = du / u
     ((ln-expr? expr)
      (let ((u (operand expr)))
        (list '/ (differentiate u var) u)))

     (else
      (error "Unknown expression type" expr)))))

;; --- Numeric Evaluation ---

(define (evaluate expr)
  (cond
    ((number? expr) expr)
    ((symbol? expr) (error "Cannot evaluate symbolic variable" expr))
    ((pair? expr)
     (let ((op (car expr)))
       (if (memq op '(sin cos exp ln))
           (let ((a (evaluate (operand expr))))
             (case op
               ((sin) (sin a)) ((cos) (cos a))
               ((exp) (exp a)) ((ln)  (log a))))
           (let ((a (evaluate (lhs expr)))
                 (b (evaluate (rhs expr))))
             (case op
               ((+) (+ a b)) ((-) (- a b)) ((*) (* a b))
               ((/) (/ a b)) ((^) (expt a b))
               (else (error "Unknown operator" op)))))))
    (else (error "Cannot evaluate" expr))))

;; --- Infix Display ---

(define (expr->infix e)
  (define (to-infix e wrap?)
    (cond
      ((number? e) (number->string e))
      ((symbol? e) (symbol->string e))
      ((pair? e)
       (let ((op (car e)))
         (if (memq op '(sin cos exp ln))
             (string-append (symbol->string op)
                            "(" (to-infix (operand e) #f) ")")
             (let ((s (string-append (to-infix (lhs e) #t)
                                     " " (symbol->string op)
                                     " " (to-infix (rhs e) #t))))
               (if wrap? (string-append "(" s ")") s)))))
      (else "?")))
  (to-infix e #f))

;; --- Demo ---

(define (run-demo)
  (define (show expr var)
    (let ((result (differentiate expr var)))
      (display "  d/d") (display var) (display "  ")
      (write expr) (newline)
      (display "     = ") (write result) (newline)
      (display "     = ") (display (expr->infix result)) (newline)
      (newline)))
  (display "--- Symbolic Differentiation Demo ---") (newline)
  (newline)
  (show '(^ x 3) 'x)
  (show '(* x x) 'x)
  (show '(sin (* 2 x)) 'x)
  (show '(+ (* x y) (^ x 2)) 'x)
  (show '(ln x) 'x)
  (show '(exp (^ x 2)) 'x)
  (show '(* (^ x 2) (sin x)) 'x))

;; --- Main ---

(define (parse-expr str)
  (read (open-input-string str)))

(let ((args (command-line)))
  (if (< (length args) 3)
      (begin
        (display "Usage: kaappi app.scm <command> [args...]") (newline)
        (display "Commands:") (newline)
        (display "  diff EXPR VAR    Differentiate EXPR with respect to VAR") (newline)
        (display "  simplify EXPR    Simplify an expression") (newline)
        (display "  eval EXPR        Evaluate a numeric expression") (newline)
        (display "  demo             Show example differentiations") (newline))
      (let ((cmd (list-ref args 2)))
        (cond
          ((equal? cmd "diff")
           (if (< (length args) 5)
               (begin
                 (display "Usage: kaappi app.scm diff EXPR VAR") (newline)
                 (display "Example: kaappi app.scm diff \"(* x x)\" x") (newline))
               (let* ((expr (parse-expr (list-ref args 3)))
                      (var  (string->symbol (list-ref args 4)))
                      (result (differentiate expr var)))
                 (display "d/d") (display var) (display "  ")
                 (write expr) (newline)
                 (display "   = ") (write result) (newline)
                 (display "   = ") (display (expr->infix result)) (newline))))

          ((equal? cmd "simplify")
           (if (< (length args) 4)
               (begin (display "Usage: kaappi app.scm simplify EXPR") (newline))
               (let* ((expr (parse-expr (list-ref args 3)))
                      (result (simplify expr)))
                 (write expr) (newline)
                 (display "   = ") (write result) (newline)
                 (display "   = ") (display (expr->infix result)) (newline))))

          ((equal? cmd "eval")
           (if (< (length args) 4)
               (begin (display "Usage: kaappi app.scm eval EXPR") (newline))
               (let* ((expr (parse-expr (list-ref args 3)))
                      (result (evaluate expr)))
                 (write expr) (display " = ") (display result) (newline))))

          ((equal? cmd "demo")
           (run-demo))

          (else
           (display "Unknown command: ") (display cmd) (newline)
           (display "Commands: diff, simplify, eval, demo") (newline))))))
