;;; Functional Sorting Algorithms
;;;
;;; Quicksort and merge sort implemented purely with higher-order
;;; functions — no mutation. Showcases closures, function composition,
;;; custom comparators, and sort-by-key abstractions.
;;;
;;; Usage:
;;;   kaappi app.scm demo
;;;   kaappi app.scm bench 5000
;;;   kaappi app.scm quick 5 3 8 1 9
;;;   kaappi app.scm merge banana apple cherry date
;;;
;;; Prerequisites: none (pure Scheme)

(import (scheme base) (scheme write) (scheme process-context)
        (scheme time) (srfi 13))

;; --- Higher-Order Primitives ---
;; Implemented from scratch to showcase closures and recursion.

(define (my-filter pred lst)
  (cond
    ((null? lst) '())
    ((pred (car lst))
     (cons (car lst) (my-filter pred (cdr lst))))
    (else (my-filter pred (cdr lst)))))

(define (my-fold-left f init lst)
  (if (null? lst) init
      (my-fold-left f (f init (car lst)) (cdr lst))))

(define (my-fold-right f init lst)
  (if (null? lst) init
      (f (car lst) (my-fold-right f init (cdr lst)))))

(define (my-partition pred lst)
  (my-fold-right
   (lambda (x acc)
     (if (pred x)
         (cons (cons x (car acc)) (cdr acc))
         (cons (car acc) (cons x (cdr acc)))))
   (cons '() '())
   lst))

;; --- Function Combinators ---

(define (compose f g)
  (lambda (x) (f (g x))))

(define (flip f)
  (lambda (a b) (f b a)))

(define (on key-fn less?)
  (lambda (a b) (less? (key-fn a) (key-fn b))))

;; --- Quicksort ---
;; Purely functional: partition into less/equal/greater, recurse, append.

(define (quicksort less? lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let* ((pivot (car lst))
             (rest (cdr lst))
             (parts (three-way-partition less? pivot rest)))
        (append (quicksort less? (car parts))
                (cons pivot (cadr parts))
                (quicksort less? (caddr parts))))))

(define (three-way-partition less? pivot lst)
  (my-fold-right
   (lambda (x acc)
     (cond
       ((less? x pivot)
        (list (cons x (car acc)) (cadr acc) (caddr acc)))
       ((less? pivot x)
        (list (car acc) (cadr acc) (cons x (caddr acc))))
       (else
        (list (car acc) (cons x (cadr acc)) (caddr acc)))))
   (list '() '() '())
   lst))

;; --- Merge Sort ---
;; Purely functional: split in half, recurse, merge. Stable sort.

(define (merge-sort less? lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let* ((halves (split-list lst))
             (left  (merge-sort less? (car halves)))
             (right (merge-sort less? (cdr halves))))
        (merge-lists less? left right))))

(define (split-list lst)
  (let loop ((slow lst) (fast lst) (acc '()))
    (if (or (null? fast) (null? (cdr fast)))
        (cons (reverse acc) slow)
        (loop (cdr slow) (cddr fast) (cons (car slow) acc)))))

(define (merge-lists less? a b)
  (cond
    ((null? a) b)
    ((null? b) a)
    ((less? (car b) (car a))
     (cons (car b) (merge-lists less? a (cdr b))))
    (else
     (cons (car a) (merge-lists less? (cdr a) b)))))

;; --- Sort-by-Key ---

(define (sort-by key-fn less? lst)
  (merge-sort (on key-fn less?) lst))

;; --- Comparator Composition ---

(define (comparing primary . tiebreakers)
  (lambda (a b)
    (let loop ((cmps (cons primary tiebreakers)))
      (cond
        ((null? cmps) #f)
        (((car cmps) a b) #t)
        (((car cmps) b a) #f)
        (else (loop (cdr cmps)))))))

;; --- Random List Generator ---
;; LCG for reproducible benchmarks.

(define *mod* 4294967296)

(define (make-rng seed)
  (list (remainder (abs seed) *mod*)))

(define (rng-next! rng)
  (let ((next (remainder (+ (* 1664525 (car rng)) 1013904223) *mod*)))
    (set-car! rng next)
    next))

(define (random-list rng n limit)
  (let loop ((i 0) (acc '()))
    (if (= i n) acc
        (loop (+ i 1) (cons (remainder (rng-next! rng) limit) acc)))))

;; --- Timing ---

(define (time-ms thunk)
  (let* ((start (current-jiffy))
         (result (thunk))
         (end (current-jiffy))
         (jps (jiffies-per-second)))
    (cons result (inexact (/ (* (- end start) 1000) jps)))))

;; --- Demo ---

(define (run-demo)
  (display "--- Functional Sorting Demo ---") (newline)
  (display "--- Quicksort & Merge Sort with Higher-Order Abstractions ---")
  (newline) (newline)

  ;; 1. Basic numeric sort
  (let ((nums '(38 27 43 3 9 82 10)))
    (display "  Numeric sort (ascending):") (newline)
    (display "  input:      ") (write nums) (newline)
    (display "  quicksort:  ") (write (quicksort < nums)) (newline)
    (display "  merge-sort: ") (write (merge-sort < nums)) (newline)
    (newline)

    (display "  Numeric sort (descending):") (newline)
    (display "  quicksort:  ") (write (quicksort > nums)) (newline)
    (newline))

  ;; 2. String sort
  (let ((words '("banana" "apple" "cherry" "date" "elderberry" "fig")))
    (display "  String sort (lexicographic):") (newline)
    (display "  input:      ") (write words) (newline)
    (display "  merge-sort: ") (write (merge-sort string<? words)) (newline)
    (newline))

  ;; 3. Sort-by-key
  (let ((words '("fig" "banana" "date" "elderberry" "apple" "cherry")))
    (display "  Sort by string length:") (newline)
    (display "  input:      ") (write words) (newline)
    (display "  sort-by:    ")
    (write (sort-by string-length < words)) (newline)
    (newline))

  (let ((pairs '((3 . "c") (1 . "a") (2 . "b") (1 . "d") (3 . "a"))))
    (display "  Sort pairs by first element:") (newline)
    (display "  input:      ") (write pairs) (newline)
    (display "  sort-by:    ") (write (sort-by car < pairs)) (newline)
    (newline))

  ;; 4. Stability demonstration
  (let ((data '((3 . "x") (1 . "a") (2 . "m") (1 . "b") (2 . "n") (3 . "y"))))
    (display "  Stability test (sort by car, preserve insertion order):") (newline)
    (display "  input:      ") (write data) (newline)
    (display "  merge-sort: ") (write (sort-by car < data)) (newline)
    (display "  quicksort:  ") (write (quicksort (on car <) data)) (newline)
    (display "  (merge sort is stable: equal keys keep original order)") (newline)
    (newline))

  ;; 5. Composed comparators
  (let ((people '(("Alice" . 30) ("Bob" . 25) ("Carol" . 30)
                  ("Dave" . 25) ("Eve" . 35))))
    (display "  Composed comparator (age ascending, then name):") (newline)
    (display "  input:      ") (write people) (newline)
    (display "  sorted:     ")
    (write (merge-sort
            (comparing (on cdr <) (on car string<?))
            people))
    (newline) (newline))

  ;; 6. Higher-order building blocks
  (display "  Higher-order primitives in action:") (newline)
  (display "  my-filter odd? '(1 2 3 4 5 6 7 8):  ")
  (write (my-filter odd? '(1 2 3 4 5 6 7 8))) (newline)
  (display "  my-fold-left + 0 '(1 2 3 4 5):       ")
  (write (my-fold-left + 0 '(1 2 3 4 5))) (newline)
  (display "  my-fold-right cons '() '(1 2 3):      ")
  (write (my-fold-right cons '() '(1 2 3))) (newline)
  (display "  my-partition even? '(1 2 3 4 5 6):    ")
  (let ((p (my-partition even? '(1 2 3 4 5 6))))
    (write (car p)) (display " / ") (write (cdr p))) (newline)
  (display "  (compose not zero?) 5:                ")
  (write ((compose not zero?) 5)) (newline)
  (display "  (flip -) 3 10:                        ")
  (write ((flip -) 3 10)) (newline)
  (newline)

  ;; 7. Quick benchmark
  (let* ((rng (make-rng 42))
         (lst (random-list rng 1000 10000))
         (tq (time-ms (lambda () (quicksort < lst))))
         (tm (time-ms (lambda () (merge-sort < lst)))))
    (display "  1000-element random list:") (newline)
    (display "  quicksort:  ") (display (cdr tq)) (display " ms") (newline)
    (display "  merge-sort: ") (display (cdr tm)) (display " ms") (newline)))

;; --- Bench ---

(define (run-bench n)
  (let* ((rng (make-rng 12345))
         (lst (random-list rng n 1000000)))
    (display "Sorting ") (display n) (display " random integers")
    (newline) (newline)

    (let ((tq (time-ms (lambda () (quicksort < lst)))))
      (display "  quicksort:  ") (display (cdr tq)) (display " ms")
      (display "  (first 10: ") (write (list-head (car tq) 10))
      (display ")") (newline))

    (let ((tm (time-ms (lambda () (merge-sort < lst)))))
      (display "  merge-sort: ") (display (cdr tm)) (display " ms")
      (display "  (first 10: ") (write (list-head (car tm) 10))
      (display ")") (newline))))

(define (list-head lst n)
  (if (or (zero? n) (null? lst)) '()
      (cons (car lst) (list-head (cdr lst) (- n 1)))))

;; --- CLI Sort ---

(define (parse-item s)
  (or (string->number s) s))

(define (all-numbers? lst)
  (cond
    ((null? lst) #t)
    ((number? (car lst)) (all-numbers? (cdr lst)))
    (else #f)))

(define (run-sort algo items)
  (let* ((sort (if (equal? algo "quick") quicksort merge-sort))
         (parsed (map parse-item items))
         (less? (if (all-numbers? parsed) < string<?)))
    (for-each (lambda (x)
                (display x) (display " "))
              (sort less? parsed))
    (newline)))

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
        (display "  demo                  Run sorting demonstrations") (newline)
        (display "  bench N               Benchmark with N random elements") (newline)
        (display "  quick ITEMS...        Quicksort the given items") (newline)
        (display "  merge ITEMS...        Merge sort the given items") (newline))
      (let ((cmd (car args)))
        (cond
          ((equal? cmd "demo")
           (run-demo))

          ((equal? cmd "bench")
           (if (< (length args) 2)
               (begin (display "Usage: kaappi app.scm bench N") (newline))
               (let ((n (string->number (cadr args))))
                 (if n
                     (run-bench n)
                     (begin (display "Invalid number: ")
                            (display (cadr args)) (newline))))))

          ((or (equal? cmd "quick") (equal? cmd "merge"))
           (if (< (length args) 2)
               (begin (display "Usage: kaappi app.scm ")
                      (display cmd) (display " ITEMS...") (newline))
               (run-sort cmd (cdr args))))

          (else
           (display "Unknown command: ") (display cmd) (newline)
           (display "Commands: demo, bench, quick, merge") (newline))))))
