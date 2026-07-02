;;; Matrix Multiplication via Accumulators
;;;
;;; Matrix operations using nested recursion with explicit accumulator
;;; parameters — no set! in any computation. Supports exact rational
;;; arithmetic and pretty-printed output.
;;;
;;; Usage:
;;;   kaappi app.scm demo
;;;   kaappi app.scm multiply "((1 2) (3 4))" "((5 6) (7 8))"
;;;   kaappi app.scm power "((1 1) (1 0))" 10
;;;
;;; Prerequisites: none (pure Scheme)

(import (scheme base) (scheme write) (scheme read)
        (scheme process-context) (scheme inexact))

;; --- Matrix Representation ---
;; A matrix is a vector of row vectors.

(define (make-matrix rows cols init)
  (let build-rows ((i 0) (acc '()))
    (if (= i rows)
        (list->vector (reverse acc))
        (build-rows (+ i 1)
                    (cons (make-vector cols init) acc)))))

(define (matrix-rows m) (vector-length m))
(define (matrix-cols m) (vector-length (vector-ref m 0)))
(define (matrix-ref m i j) (vector-ref (vector-ref m i) j))
(define (matrix-set! m i j v) (vector-set! (vector-ref m i) j v))

(define (matrix-identity n)
  (let ((m (make-matrix n n 0)))
    (let fill ((i 0))
      (when (< i n)
        (matrix-set! m i i 1)
        (fill (+ i 1))))
    m))

(define (list->matrix lst)
  (let ((rows (map list->vector lst)))
    (list->vector rows)))

(define (matrix->list m)
  (let rows->list ((i 0) (acc '()))
    (if (= i (matrix-rows m))
        (reverse acc)
        (rows->list (+ i 1)
                    (cons (vector->list (vector-ref m i)) acc)))))

;; --- Core Operations (Recursive Accumulators) ---

(define (dot-product v1 v2 len)
  (let sum ((k 0) (acc 0))
    (if (= k len)
        acc
        (sum (+ k 1)
             (+ acc (* (vector-ref v1 k) (vector-ref v2 k)))))))

(define (matrix-multiply a b)
  (let ((ar (matrix-rows a))
        (ac (matrix-cols a))
        (bc (matrix-cols b)))
    (let build-rows ((i 0) (rows '()))
      (if (= i ar)
          (list->vector (reverse rows))
          (build-rows
           (+ i 1)
           (cons (let build-cols ((j 0) (cols '()))
                   (if (= j bc)
                       (list->vector (reverse cols))
                       (build-cols
                        (+ j 1)
                        (cons (let sum-k ((k 0) (acc 0))
                                (if (= k ac)
                                    acc
                                    (sum-k (+ k 1)
                                           (+ acc (* (matrix-ref a i k)
                                                     (matrix-ref b k j))))))
                              cols))))
                 rows))))))

(define (matrix-add a b)
  (let ((r (matrix-rows a)) (c (matrix-cols a)))
    (let build-rows ((i 0) (rows '()))
      (if (= i r)
          (list->vector (reverse rows))
          (build-rows
           (+ i 1)
           (cons (let build-cols ((j 0) (cols '()))
                   (if (= j c)
                       (list->vector (reverse cols))
                       (build-cols (+ j 1)
                                   (cons (+ (matrix-ref a i j)
                                            (matrix-ref b i j))
                                         cols))))
                 rows))))))

(define (matrix-scale s m)
  (let ((r (matrix-rows m)) (c (matrix-cols m)))
    (let build-rows ((i 0) (rows '()))
      (if (= i r)
          (list->vector (reverse rows))
          (build-rows
           (+ i 1)
           (cons (let build-cols ((j 0) (cols '()))
                   (if (= j c)
                       (list->vector (reverse cols))
                       (build-cols (+ j 1)
                                   (cons (* s (matrix-ref m i j))
                                         cols))))
                 rows))))))

(define (matrix-transpose m)
  (let ((r (matrix-rows m)) (c (matrix-cols m)))
    (let build-rows ((j 0) (rows '()))
      (if (= j c)
          (list->vector (reverse rows))
          (build-rows
           (+ j 1)
           (cons (let build-cols ((i 0) (cols '()))
                   (if (= i r)
                       (list->vector (reverse cols))
                       (build-cols (+ i 1)
                                   (cons (matrix-ref m i j) cols))))
                 rows))))))

(define (matrix-power m n)
  (cond
    ((= n 0) (matrix-identity (matrix-rows m)))
    ((= n 1) m)
    ((even? n)
     (let ((half (matrix-power m (/ n 2))))
       (matrix-multiply half half)))
    (else
     (matrix-multiply m (matrix-power m (- n 1))))))

(define (matrix-equal? a b)
  (let ((r (matrix-rows a)) (c (matrix-cols a)))
    (let check-rows ((i 0))
      (if (= i r) #t
          (let check-cols ((j 0))
            (if (= j c)
                (check-rows (+ i 1))
                (if (equal? (matrix-ref a i j) (matrix-ref b i j))
                    (check-cols (+ j 1))
                    #f)))))))

;; --- Pretty Printer ---

(define (number->display-string n)
  (if (and (rational? n) (not (integer? n)) (exact? n))
      (string-append (number->string (numerator n))
                     "/"
                     (number->string (denominator n)))
      (number->string n)))

(define (matrix-display m)
  (let* ((r (matrix-rows m))
         (c (matrix-cols m))
         (strs (let build-rows ((i 0) (rows '()))
                 (if (= i r)
                     (reverse rows)
                     (build-rows
                      (+ i 1)
                      (cons (let build-cols ((j 0) (cols '()))
                              (if (= j c)
                                  (reverse cols)
                                  (build-cols (+ j 1)
                                              (cons (number->display-string
                                                     (matrix-ref m i j))
                                                    cols))))
                            rows)))))
         (widths (let find-widths ((j 0) (acc '()))
                   (if (= j c)
                       (reverse acc)
                       (find-widths
                        (+ j 1)
                        (cons (let max-col ((rows strs) (w 0))
                                (if (null? rows)
                                    w
                                    (max-col (cdr rows)
                                             (max w (string-length
                                                     (list-ref (car rows) j))))))
                              acc))))))
    (for-each
     (lambda (row)
       (display "  |")
       (for-each
        (lambda (s w)
          (display " ")
          (display (make-string (- w (string-length s)) #\space))
          (display s))
        row widths)
       (display " |") (newline))
     strs)))

;; --- Demo ---

(define (run-demo)
  (display "--- Matrix Math Demo ---") (newline)
  (display "--- Nested Recursion with Accumulators ---") (newline)
  (newline)

  ;; 1. Identity multiplication
  (display "  1. Identity: A * I = A") (newline)
  (let ((a (list->matrix '((1 2 3) (4 5 6) (7 8 9))))
        (i3 (matrix-identity 3)))
    (display "  A:") (newline) (matrix-display a)
    (display "  A * I:") (newline) (matrix-display (matrix-multiply a i3))
    (display "  Equal? ") (display (matrix-equal? a (matrix-multiply a i3)))
    (newline) (newline))

  ;; 2. 2x2 multiplication
  (display "  2. Basic 2x2 multiplication") (newline)
  (let ((a (list->matrix '((1 2) (3 4))))
        (b (list->matrix '((5 6) (7 8)))))
    (display "  A:") (newline) (matrix-display a)
    (display "  B:") (newline) (matrix-display b)
    (display "  A * B:") (newline)
    (matrix-display (matrix-multiply a b))
    (display "  (expected: ((19 22) (43 50)))") (newline)
    (newline))

  ;; 3. 3x3 multiplication
  (display "  3. 3x3 multiplication") (newline)
  (let ((a (list->matrix '((2 0 1) (3 0 0) (5 1 1))))
        (b (list->matrix '((1 0 1) (1 2 1) (1 1 0)))))
    (display "  A:") (newline) (matrix-display a)
    (display "  B:") (newline) (matrix-display b)
    (display "  A * B:") (newline)
    (matrix-display (matrix-multiply a b))
    (newline))

  ;; 4. Fibonacci via matrix exponentiation
  (display "  4. Fibonacci via matrix power") (newline)
  (let ((fib-mat (list->matrix '((1 1) (1 0)))))
    (display "  F = ((1 1) (1 0))") (newline)
    (let show ((n 2))
      (when (<= n 12)
        (let* ((result (matrix-power fib-mat n))
               (fib-n (matrix-ref result 0 1)))
          (display "  F^") (display n)
          (display " => fib(") (display n) (display ") = ")
          (display fib-n) (newline))
        (show (+ n 1))))
    (newline))

  ;; 5. Transpose property: (AB)^T = B^T * A^T
  (display "  5. Transpose: (AB)^T = B^T * A^T") (newline)
  (let* ((a (list->matrix '((1 2) (3 4) (5 6))))
         (b (list->matrix '((7 8 9) (10 11 12))))
         (ab (matrix-multiply a b))
         (lhs (matrix-transpose ab))
         (rhs (matrix-multiply (matrix-transpose b)
                               (matrix-transpose a))))
    (display "  (AB)^T:") (newline) (matrix-display lhs)
    (display "  B^T * A^T:") (newline) (matrix-display rhs)
    (display "  Equal? ") (display (matrix-equal? lhs rhs))
    (newline) (newline))

  ;; 6. Exact rational arithmetic
  (display "  6. Exact rational matrices") (newline)
  (let* ((a (list->matrix (list (list (/ 1 2) (/ 1 3))
                                (list (/ 1 4) (/ 1 5)))))
         (b (list->matrix (list (list 2 3) (list 4 5)))))
    (display "  A:") (newline) (matrix-display a)
    (display "  B:") (newline) (matrix-display b)
    (display "  A * B:") (newline)
    (matrix-display (matrix-multiply a b))
    (display "  (exact fractions preserved)") (newline)
    (newline))

  ;; 7. Associativity: (AB)C = A(BC)
  (display "  7. Associativity: (AB)C = A(BC)") (newline)
  (let* ((a (list->matrix '((1 2) (3 4))))
         (b (list->matrix '((5 6) (7 8))))
         (c (list->matrix '((9 10) (11 12))))
         (lhs (matrix-multiply (matrix-multiply a b) c))
         (rhs (matrix-multiply a (matrix-multiply b c))))
    (display "  (AB)C:") (newline) (matrix-display lhs)
    (display "  A(BC):") (newline) (matrix-display rhs)
    (display "  Equal? ") (display (matrix-equal? lhs rhs))
    (newline)))

;; --- Main ---

(define (parse-matrix str)
  (list->matrix (read (open-input-string str))))

(let ((args (command-line)))
  (if (< (length args) 2)
      (begin
        (display "Usage: kaappi app.scm <command> [args...]") (newline)
        (display "Commands:") (newline)
        (display "  demo                   Run matrix demonstrations") (newline)
        (display "  multiply A B           Multiply two matrices") (newline)
        (display "  power M N              Compute M^N") (newline))
      (let ((cmd (list-ref args 1)))
        (cond
          ((equal? cmd "demo")
           (run-demo))

          ((equal? cmd "multiply")
           (if (< (length args) 4)
               (begin
                 (display "Usage: kaappi app.scm multiply A B") (newline)
                 (display "Example: kaappi app.scm multiply ")
                 (display "\"((1 2) (3 4))\" \"((5 6) (7 8))\"") (newline))
               (let* ((a (parse-matrix (list-ref args 2)))
                      (b (parse-matrix (list-ref args 3)))
                      (result (matrix-multiply a b)))
                 (display "A:") (newline) (matrix-display a)
                 (display "B:") (newline) (matrix-display b)
                 (display "A * B:") (newline) (matrix-display result))))

          ((equal? cmd "power")
           (if (< (length args) 4)
               (begin
                 (display "Usage: kaappi app.scm power M N") (newline)
                 (display "Example: kaappi app.scm power ")
                 (display "\"((1 1) (1 0))\" 10") (newline))
               (let* ((m (parse-matrix (list-ref args 2)))
                      (n (string->number (list-ref args 3)))
                      (result (matrix-power m n)))
                 (display "M:") (newline) (matrix-display m)
                 (display "M^") (display n) (display ":") (newline)
                 (matrix-display result))))

          (else
           (display "Unknown command: ") (display cmd) (newline)
           (display "Commands: demo, multiply, power") (newline))))))
