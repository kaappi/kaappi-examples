;;; Parallel Prime Search
;;;
;;; Counts primes below N by trial division, splitting the range into one
;;; chunk per available processor and running the chunks on a
;;; (kaappi parallel) pool to show real multi-core speedup on CPU-bound
;;; work (KEP-0002 Phase 5).
;;;
;;; Chunked on purpose rather than one task per number: (kaappi parallel)'s
;;; pool/parallel-map currently inherits open correctness issues in the
;;; cross-thread channel wakeup path (kaappi#1487, kaappi#1489) that show up
;;; as intermittent hangs somewhere past a few hundred concurrent
;;; submissions. A handful of chunk-sized tasks (one per processor,
;;; regardless of how large N is) stays well clear of that, and is also
;;; just a more efficient shape for this kind of work.
;;;
;;; Usage:
;;;   kaappi app.scm demo
;;;   kaappi app.scm count 200000
;;;
;;; Prerequisites: none (pure Scheme)

(import (scheme base) (scheme write) (scheme process-context)
        (scheme time) (srfi 1) (srfi 13) (kaappi parallel))

;; --- Primality (trial division) ---

(define (prime? n)
  (cond ((< n 2) #f)
        ((= n 2) #t)
        ((even? n) #f)
        (else
         (let loop ((d 3))
           (cond ((> (* d d) n) #t)
                 ((zero? (remainder n d)) #f)
                 (else (loop (+ d 2))))))))

(define (count-primes-in-range lo hi)
  (let loop ((n lo) (count 0))
    (if (>= n hi)
        count
        (loop (+ n 1) (if (prime? n) (+ count 1) count)))))

;; --- Chunking: one task per processor, not one per number ---

(define (chunk-bounds n chunks)
  (let ((size (quotient n chunks)))
    (map (lambda (i)
           (cons (+ 2 (* i size))
                 (if (= i (- chunks 1)) n (+ 2 (* (+ i 1) size)))))
         (iota chunks))))

(define (parallel-count-primes n workers)
  (let* ((pool (make-pool workers))
         (bounds (chunk-bounds n workers))
         (replies (map (lambda (b)
                         (pool-submit pool (lambda () (count-primes-in-range (car b) (cdr b)))))
                       bounds))
         (total (apply + (map task-wait replies))))
    (pool-shutdown! pool)
    total))

;; --- Timing ---

(define (elapsed-seconds thunk)
  (let* ((t0 (current-jiffy))
         (result (thunk))
         (t1 (current-jiffy)))
    (values result (/ (- t1 t0) (inexact (jiffies-per-second))))))

;; --- Commands ---

(define (run-count n)
  (display "primes below ") (display n) (display ": ")
  (display (parallel-count-primes n (processor-count)))
  (newline))

(define (run-demo)
  (let ((n 200000) (workers (processor-count)))
    (display "Counting primes below ") (display n)
    (display " (") (display workers) (display " processors available)")
    (newline)
    (let*-values (((seq-count seq-time)
                   (elapsed-seconds (lambda () (count-primes-in-range 2 n))))
                  ((par-count par-time)
                   (elapsed-seconds (lambda () (parallel-count-primes n workers)))))
      (display "  sequential:            ") (display seq-count)
      (display " primes in ") (display seq-time) (display "s") (newline)
      (display "  parallel (") (display workers) (display " chunks): ")
      (display par-count)
      (display " primes in ") (display par-time) (display "s") (newline)
      (when (> par-time 0)
        (display "  speedup: ") (display (/ seq-time par-time)) (display "x") (newline)))))

;; --- Main ---

(define (user-args)
  (let ((args (command-line)))
    (cond
      ((null? args) '())
      ((string-suffix? ".scm" (car args)) (cdr args))
      ((and (pair? (cdr args)) (string-suffix? ".scm" (cadr args)))
       (cddr args))
      (else (cdr args)))))

(let ((args (user-args)))
  (if (null? args)
      (begin
        (display "Usage: kaappi app.scm <command> [args...]") (newline)
        (display "Commands:") (newline)
        (display "  demo       Compare sequential vs parallel-pool prime counting") (newline)
        (display "  count N    Count primes below N using a parallel pool") (newline))
      (let ((cmd (car args)))
        (cond
          ((equal? cmd "demo")
           (run-demo))

          ((equal? cmd "count")
           (if (< (length args) 2)
               (begin
                 (display "Usage: kaappi app.scm count N") (newline)
                 (display "Example: kaappi app.scm count 200000") (newline))
               (run-count (string->number (cadr args)))))

          (else
           (display "Unknown command: ") (display cmd) (newline)
           (display "Commands: demo, count") (newline))))))
