;;; Maze Solver with Backtracking
;;;
;;; Generates random mazes using recursive backtracker (DFS) and solves
;;; them with DFS backtracking over immutable visited sets. Renders
;;; ASCII art with solution path overlay.
;;;
;;; Usage:
;;;   kaappi app.scm demo
;;;   kaappi app.scm generate 15 10
;;;   kaappi app.scm solve 15 10
;;;   kaappi app.scm solve 15 10 42
;;;
;;; Prerequisites: none (pure Scheme)

(import (scheme base) (scheme write) (scheme process-context)
        (scheme inexact) (srfi 151))

;; --- Random Number Generator ---
;; LCG with Numerical Recipes constants (mod 2^32)

(define *mod* 4294967296)

(define (make-rng seed)
  (list (remainder (abs seed) *mod*)))

(define (rng-next! rng)
  (let ((next (remainder (+ (* 1664525 (car rng)) 1013904223) *mod*)))
    (set-car! rng next)
    next))

(define (rng-range! rng n)
  (remainder (rng-next! rng) n))

;; Fisher-Yates shuffle
(define (shuffle! rng lst)
  (let ((v (list->vector lst)))
    (do ((i (- (vector-length v) 1) (- i 1)))
        ((<= i 0) (vector->list v))
      (let* ((j (rng-range! rng (+ i 1)))
             (tmp (vector-ref v i)))
        (vector-set! v i (vector-ref v j))
        (vector-set! v j tmp)))))

;; --- Maze Representation ---
;; Grid stored as flat vector of wall bitmasks.
;; Each cell stores which walls have been REMOVED (passages).
;; N=1, E=2, S=4, W=8

(define N 1) (define E 2) (define S 4) (define W 8)

(define (opposite dir)
  (cond ((= dir N) S) ((= dir S) N)
        ((= dir E) W) ((= dir W) E)
        (else 0)))

(define (dir-delta dir)
  (cond ((= dir N) (cons  0 -1))
        ((= dir S) (cons  0  1))
        ((= dir E) (cons  1  0))
        ((= dir W) (cons -1  0))
        (else (cons 0 0))))

(define (make-grid cols rows)
  (let ((grid (make-vector (* cols rows) 0)))
    (list grid cols rows)))

(define (grid-vec g)  (car g))
(define (grid-cols g) (cadr g))
(define (grid-rows g) (caddr g))

(define (grid-idx g x y)
  (+ (* y (grid-cols g)) x))

(define (grid-ref g x y)
  (vector-ref (grid-vec g) (grid-idx g x y)))

(define (grid-set! g x y val)
  (vector-set! (grid-vec g) (grid-idx g x y) val))

(define (in-bounds? g x y)
  (and (>= x 0) (< x (grid-cols g))
       (>= y 0) (< y (grid-rows g))))

(define (has-passage? g x y dir)
  (not (zero? (bitwise-and (grid-ref g x y) dir))))

;; --- Maze Generation ---
;; Recursive backtracker: DFS carving passages through the grid.

(define (carve! g rng x y)
  (let ((dirs (shuffle! rng (list N E S W))))
    (for-each
     (lambda (dir)
       (let* ((d (dir-delta dir))
              (nx (+ x (car d)))
              (ny (+ y (cdr d))))
         (when (and (in-bounds? g nx ny)
                    (zero? (grid-ref g nx ny)))
           (grid-set! g x y (bitwise-ior (grid-ref g x y) dir))
           (grid-set! g nx ny (bitwise-ior (grid-ref g nx ny) (opposite dir)))
           (carve! g rng nx ny))))
     dirs)))

(define (generate-maze cols rows seed)
  (let ((g (make-grid cols rows))
        (rng (make-rng seed)))
    (carve! g rng 0 0)
    g))

;; --- Solver ---
;; DFS with immutable visited set (list of (x . y) pairs).
;; Returns the path as a list of (x . y) or #f if no path exists.

(define (coord-member? x y visited)
  (cond
    ((null? visited) #f)
    ((and (= x (caar visited)) (= y (cdar visited))) #t)
    (else (coord-member? x y (cdr visited)))))

(define (solve-maze g)
  (let ((goal-x (- (grid-cols g) 1))
        (goal-y (- (grid-rows g) 1)))
    (let dfs ((x 0) (y 0) (visited '()))
      (cond
        ((coord-member? x y visited) #f)
        ((and (= x goal-x) (= y goal-y))
         (list (cons x y)))
        (else
         (let ((visited (cons (cons x y) visited)))
           (let try ((dirs (list N E S W)))
             (if (null? dirs)
                 #f
                 (if (has-passage? g x y (car dirs))
                     (let* ((d (dir-delta (car dirs)))
                            (nx (+ x (car d)))
                            (ny (+ y (cdr d)))
                            (result (dfs nx ny visited)))
                       (if result
                           (cons (cons x y) result)
                           (try (cdr dirs))))
                     (try (cdr dirs)))))))))))

;; --- ASCII Renderer ---

(define (path->set path)
  (if path path '()))

(define (on-path? path-set x y)
  (coord-member? x y path-set))

(define (render-maze g path)
  (let ((cols (grid-cols g))
        (rows (grid-rows g))
        (path-set (path->set path)))

    ;; Top border
    (display "+")
    (do ((x 0 (+ x 1))) ((= x cols))
      (display "---+"))
    (newline)

    ;; Each row: cell line then bottom border line
    (do ((y 0 (+ y 1))) ((= y rows))

      ;; Cell contents and east walls
      (display "|")
      (do ((x 0 (+ x 1))) ((= x cols))
        (if (on-path? path-set x y)
            (display " * ")
            (display "   "))
        (if (and (< x (- cols 1)) (has-passage? g x y E))
            (display " ")
            (display "|")))
      (newline)

      ;; South walls
      (display "+")
      (do ((x 0 (+ x 1))) ((= x cols))
        (if (and (< y (- rows 1)) (has-passage? g x y S))
            (display "   +")
            (display "---+")))
      (newline))))

;; --- Demo ---

(define (run-demo)
  (display "--- Maze Solver Demo ---") (newline)
  (display "--- DFS generation + backtracking solver ---") (newline)
  (newline)

  (display "  5x5 maze (seed 42):") (newline) (newline)
  (let* ((g (generate-maze 5 5 42))
         (path (solve-maze g)))
    (render-maze g path)
    (newline)
    (display "  Path: ")
    (for-each (lambda (p)
                (display "(") (display (car p)) (display ",")
                (display (cdr p)) (display ") "))
              path)
    (newline) (newline))

  (display "  10x10 maze (seed 7):") (newline) (newline)
  (let* ((g (generate-maze 10 10 7))
         (path (solve-maze g)))
    (render-maze g path)
    (newline)
    (display "  Path length: ") (display (length path))
    (display " steps") (newline) (newline))

  (display "  15x15 maze (seed 123):") (newline) (newline)
  (let* ((g (generate-maze 15 15 123))
         (path (solve-maze g)))
    (render-maze g #f)
    (newline)
    (display "  Solved maze:") (newline) (newline)
    (render-maze g path)
    (newline)
    (display "  Path length: ") (display (length path))
    (display " steps") (newline)))

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
        (display "  demo               Generate and solve example mazes") (newline)
        (display "  generate W H [S]   Generate a WxH maze (optional seed S)") (newline)
        (display "  solve W H [S]      Generate and solve a WxH maze") (newline))
      (let ((cmd (car args)))
        (cond
          ((equal? cmd "demo")
           (run-demo))

          ((equal? cmd "generate")
           (if (< (length args) 3)
               (begin
                 (display "Usage: kaappi app.scm generate WIDTH HEIGHT [SEED]") (newline))
               (let* ((w (string->number (cadr args)))
                      (h (string->number (caddr args)))
                      (seed (if (> (length args) 3)
                                (string->number (cadddr args))
                                42))
                      (g (generate-maze w h seed)))
                 (render-maze g #f))))

          ((equal? cmd "solve")
           (if (< (length args) 3)
               (begin
                 (display "Usage: kaappi app.scm solve WIDTH HEIGHT [SEED]") (newline))
               (let* ((w (string->number (cadr args)))
                      (h (string->number (caddr args)))
                      (seed (if (> (length args) 3)
                                (string->number (cadddr args))
                                42))
                      (g (generate-maze w h seed))
                      (path (solve-maze g)))
                 (if path
                     (begin
                       (render-maze g path)
                       (newline)
                       (display "Path length: ") (display (length path))
                       (display " steps") (newline))
                     (begin
                       (render-maze g #f)
                       (newline)
                       (display "No path found!") (newline))))))

          (else
           (display "Unknown command: ") (display cmd) (newline)
           (display "Commands: demo, generate, solve") (newline))))))
