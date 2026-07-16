;;; Conway's Game of Life
;;;
;;; Cellular automaton on a toroidal grid. Each generation builds a
;;; fresh grid from the previous one — the source is never mutated
;;; during computation. Includes classic patterns (glider, blinker,
;;; Gosper glider gun, R-pentomino) and animated terminal display.
;;;
;;; Usage:
;;;   kaappi app.scm demo
;;;   kaappi app.scm glider 20 10 30
;;;   kaappi app.scm gun 40 20 100
;;;   kaappi app.scm random 30 20 50 42 30
;;;   kaappi app.scm animate glider 20 10 50
;;;
;;; Prerequisites: none (pure Scheme)

(import (scheme base) (scheme write) (scheme process-context)
        (scheme time) (srfi 13) (srfi 18))

;; --- Random Number Generator ---

(define *mod* 4294967296)

(define (make-rng seed)
  (list (remainder (abs seed) *mod*)))

(define (rng-next! rng)
  (let ((next (remainder (+ (* 1664525 (car rng)) 1013904223) *mod*)))
    (set-car! rng next)
    next))

(define (rng-range! rng n)
  (remainder (rng-next! rng) n))

;; --- Grid Representation ---
;; Vector of row vectors, each cell 0 (dead) or 1 (alive).
;; Rows are allocated individually — (make-vector rows (make-vector ...))
;; would share the same inner vector across all rows.

(define (make-grid rows cols)
  (let ((g (make-vector rows #f)))
    (do ((r 0 (+ r 1))) ((= r rows) g)
      (vector-set! g r (make-vector cols 0)))))

(define (grid-rows g) (vector-length g))
(define (grid-cols g) (vector-length (vector-ref g 0)))
(define (grid-ref g r c) (vector-ref (vector-ref g r) c))
(define (grid-set! g r c v) (vector-set! (vector-ref g r) c v))

(define (random-grid rows cols rng density)
  (let ((g (make-grid rows cols)))
    (do ((r 0 (+ r 1))) ((= r rows) g)
      (do ((c 0 (+ c 1))) ((= c cols))
        (when (< (rng-range! rng 100) density)
          (grid-set! g r c 1))))))

;; --- Game Rules ---

(define *deltas*
  '((-1 . -1) (-1 . 0) (-1 . 1)
    ( 0 . -1)           ( 0 . 1)
    ( 1 . -1) ( 1 . 0)  ( 1 . 1)))

(define (wrap v limit)
  (let ((m (remainder v limit)))
    (if (< m 0) (+ m limit) m)))

(define (count-neighbors grid r c)
  (let ((rows (grid-rows grid))
        (cols (grid-cols grid)))
    (let count ((ds *deltas*) (n 0))
      (if (null? ds) n
          (count (cdr ds)
                 (+ n (grid-ref grid
                                (wrap (+ r (caar ds)) rows)
                                (wrap (+ c (cdar ds)) cols))))))))

(define (next-generation grid)
  (let* ((rows (grid-rows grid))
         (cols (grid-cols grid))
         (new (make-grid rows cols)))
    (do ((r 0 (+ r 1))) ((= r rows) new)
      (do ((c 0 (+ c 1))) ((= c cols))
        (let ((alive (grid-ref grid r c))
              (nbrs (count-neighbors grid r c)))
          (grid-set! new r c
            (cond
              ((and (= alive 1) (or (= nbrs 2) (= nbrs 3))) 1)
              ((and (= alive 0) (= nbrs 3)) 1)
              (else 0))))))))

;; --- Pattern Library ---

(define (place-cells! grid r0 c0 offsets)
  (let ((rows (grid-rows grid))
        (cols (grid-cols grid)))
    (for-each
     (lambda (off)
       (grid-set! grid
                  (wrap (+ r0 (car off)) rows)
                  (wrap (+ c0 (cdr off)) cols)
                  1))
     offsets)))

(define (place-glider! grid r0 c0)
  (place-cells! grid r0 c0
    '((0 . 1) (1 . 2) (2 . 0) (2 . 1) (2 . 2))))

(define (place-blinker! grid r0 c0)
  (place-cells! grid r0 c0
    '((0 . 0) (0 . 1) (0 . 2))))

(define (place-r-pentomino! grid r0 c0)
  (place-cells! grid r0 c0
    '((0 . 1) (0 . 2) (1 . 0) (1 . 1) (2 . 1))))

(define (place-gun! grid r0 c0)
  (place-cells! grid r0 c0
    '((0 . 24)
      (1 . 22) (1 . 24)
      (2 . 12) (2 . 13) (2 . 20) (2 . 21) (2 . 34) (2 . 35)
      (3 . 11) (3 . 15) (3 . 20) (3 . 21) (3 . 34) (3 . 35)
      (4 .  0) (4 .  1) (4 . 10) (4 . 16) (4 . 20) (4 . 21)
      (5 .  0) (5 .  1) (5 . 10) (5 . 14) (5 . 16) (5 . 17) (5 . 22) (5 . 24)
      (6 . 10) (6 . 16) (6 . 24)
      (7 . 11) (7 . 15)
      (8 . 12) (8 . 13))))

(define (make-pattern-grid pattern rows cols . args)
  (cond
    ((equal? pattern "random")
     (let ((seed (if (pair? args) (car args) 42))
           (density (if (and (pair? args) (pair? (cdr args)))
                        (cadr args) 30)))
       (random-grid rows cols (make-rng seed) density)))
    (else
     (let ((g (make-grid rows cols))
           (r0 (quotient rows 3))
           (c0 (quotient cols 3)))
       (cond
         ((equal? pattern "glider")  (place-glider! g r0 c0))
         ((equal? pattern "blinker") (place-blinker! g r0 c0))
         ((equal? pattern "gun")     (place-gun! g 1 1))
         ((equal? pattern "rpent")   (place-r-pentomino! g r0 c0))
         (else (error "Unknown pattern" pattern)))
       g))))

;; --- Display ---

(define esc (string (integer->char 27)))

(define (clear-screen)
  (display (string-append esc "[2J" esc "[H"))
  (flush-output-port))

(define (count-alive grid)
  (let ((rows (grid-rows grid))
        (cols (grid-cols grid)))
    (let count ((r 0) (total 0))
      (if (= r rows) total
          (let count-row ((c 0) (row-total 0))
            (if (= c cols)
                (count (+ r 1) (+ total row-total))
                (count-row (+ c 1)
                           (+ row-total (grid-ref grid r c)))))))))

(define (display-grid grid gen)
  (display "  Generation ") (display gen)
  (display "  (population: ") (display (count-alive grid))
  (display ")") (newline)
  (let ((rows (grid-rows grid))
        (cols (grid-cols grid)))
    (do ((r 0 (+ r 1))) ((= r rows))
      (display "  ")
      (do ((c 0 (+ c 1))) ((= c cols))
        (display (if (= (grid-ref grid r c) 1) "#" ".")))
      (newline))))

(define (sleep-ms ms)
  (thread-sleep! (/ ms 1000)))

(define (time-ms thunk)
  (let* ((start (current-jiffy))
         (result (thunk))
         (end (current-jiffy))
         (jps (jiffies-per-second)))
    (cons result (inexact (/ (* (- end start) 1000) jps)))))

;; --- Simulation ---

(define (run-simulation grid n-gens show-every)
  (let loop ((g grid) (gen 0))
    (when (and show-every
               (or (= gen 0)
                   (= (remainder gen show-every) 0)
                   (= gen n-gens)))
      (display-grid g gen)
      (newline))
    (if (= gen n-gens) g
        (loop (next-generation g) (+ gen 1)))))

(define (run-animated grid n-gens delay-ms)
  (let loop ((g grid) (gen 0))
    (clear-screen)
    (display "Conway's Game of Life") (newline)
    (display-grid g gen)
    (if (= gen n-gens) g
        (begin (sleep-ms delay-ms)
               (loop (next-generation g) (+ gen 1))))))

;; --- Demo ---

(define (run-demo)
  (display "--- Conway's Game of Life Demo ---") (newline)
  (display "--- Functional Grid Updates, Toroidal Wrapping ---") (newline)
  (newline)

  ;; 1. Blinker
  (display "  1. Blinker oscillator (period 2)") (newline)
  (let ((g (make-pattern-grid "blinker" 5 5)))
    (display-grid g 0) (newline)
    (display-grid (next-generation g) 1) (newline)
    (display-grid (next-generation (next-generation g)) 2) (newline))

  ;; 2. Glider
  (display "  2. Glider (moves diagonally every 4 generations)") (newline)
  (let ((g (make-pattern-grid "glider" 10 10)))
    (display-grid g 0)
    (let ((g4 (run-simulation g 4 #f)))
      (display-grid g4 4)) (newline))

  ;; 3. R-pentomino
  (display "  3. R-pentomino (chaotic growth from 5 cells)") (newline)
  (let ((g (make-pattern-grid "rpent" 20 20)))
    (display-grid g 0)
    (let ((g10 (run-simulation g 10 #f)))
      (display-grid g10 10)) (newline))

  ;; 4. Gosper glider gun
  (display "  4. Gosper glider gun (emits a glider every 30 gens)") (newline)
  (let ((g (make-pattern-grid "gun" 20 40)))
    (display-grid g 0)
    (let ((g60 (run-simulation g 60 #f)))
      (display-grid g60 60)) (newline))

  ;; 5. Random grid
  (display "  5. Random grid (seed 42, density 30%, 15x15)") (newline)
  (let ((g (make-pattern-grid "random" 15 15 42 30)))
    (display-grid g 0)
    (let ((g20 (run-simulation g 20 #f)))
      (display-grid g20 20)) (newline))

  ;; 6. Performance
  (display "  6. Performance: 100 generations on 50x50 grid") (newline)
  (let* ((g (make-pattern-grid "random" 50 50 99 25))
         (result (time-ms (lambda () (run-simulation g 100 #f)))))
    (display "  ") (display (cdr result)) (display " ms") (newline)
    (display "  Final population: ")
    (display (count-alive (car result))) (newline)))

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
        (display "  demo                          Run demonstrations") (newline)
        (display "  glider W H N                  Glider on WxH grid, N gens") (newline)
        (display "  gun W H N                     Gosper glider gun") (newline)
        (display "  random W H N [SEED] [DENSITY] Random initial state") (newline)
        (display "  animate PATTERN W H N         Animated (glider/blinker/gun/rpent/random)") (newline))
      (let ((cmd (car args)))
        (cond
          ((equal? cmd "demo")
           (run-demo))

          ((equal? cmd "glider")
           (if (< (length args) 4)
               (begin
                 (display "Usage: kaappi app.scm glider W H N") (newline))
               (let ((w (string->number (cadr args)))
                     (h (string->number (caddr args)))
                     (n (string->number (cadddr args))))
                 (run-simulation (make-pattern-grid "glider" h w) n 1)
                 ;; return unspecified so script mode doesn't echo the grid
                 (if #f #f))))

          ((equal? cmd "gun")
           (if (< (length args) 4)
               (begin
                 (display "Usage: kaappi app.scm gun W H N") (newline))
               (let ((w (string->number (cadr args)))
                     (h (string->number (caddr args)))
                     (n (string->number (cadddr args))))
                 (run-simulation (make-pattern-grid "gun" h w) n 10)
                 (if #f #f))))

          ((equal? cmd "random")
           (if (< (length args) 4)
               (begin
                 (display "Usage: kaappi app.scm random W H N [SEED] [DENSITY]")
                 (newline))
               (let* ((w (string->number (cadr args)))
                      (h (string->number (caddr args)))
                      (n (string->number (cadddr args)))
                      (rest (list-tail args 4))
                      (seed (if (pair? rest)
                                (or (string->number (car rest)) 42) 42))
                      (density (if (and (pair? rest) (pair? (cdr rest)))
                                   (or (string->number (cadr rest)) 30) 30)))
                 (run-simulation
                  (make-pattern-grid "random" h w seed density)
                  n 10)
                 (if #f #f))))

          ((equal? cmd "animate")
           (if (< (length args) 5)
               (begin
                 (display "Usage: kaappi app.scm animate PATTERN W H N") (newline)
                 (display "Patterns: glider, blinker, gun, rpent, random") (newline))
               (let ((pat (cadr args))
                     (w (string->number (caddr args)))
                     (h (string->number (cadddr args)))
                     (n (string->number (list-ref args 4))))
                 (run-animated (make-pattern-grid pat h w) n 100)
                 (if #f #f))))

          (else
           (display "Unknown command: ") (display cmd) (newline)
           (display "Commands: demo, glider, gun, random, animate") (newline))))))
