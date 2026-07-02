;;; Huffman Coding
;;;
;;; Builds optimal prefix codes from character frequencies using a
;;; binary tree. Encodes and decodes messages with bit-level
;;; compression. Demonstrates tree construction, traversal, and
;;; pairwise merging.
;;;
;;; Usage:
;;;   kaappi app.scm demo
;;;   kaappi app.scm encode "ABRACADABRA"
;;;   kaappi app.scm analyze "hello world"
;;;
;;; Prerequisites: none (pure Scheme)

(import (scheme base) (scheme write) (scheme read)
        (scheme process-context) (scheme char))

;; --- Huffman Tree ---
;; Leaf: (leaf char weight)
;; Branch: (branch left right weight)

(define (make-leaf ch w)    (list 'leaf ch w))
(define (make-branch l r w) (list 'branch l r w))

(define (leaf? node)        (eq? (car node) 'leaf))
(define (leaf-char node)    (cadr node))
(define (node-weight node)  (caddr node))
(define (branch-left node)  (cadr node))
(define (branch-right node) (caddr node))
(define (branch-weight node)(cadddr node))

(define (node-weight* node)
  (if (leaf? node) (node-weight node) (branch-weight node)))

;; --- Frequency Counting ---

(define (count-frequencies str)
  (let ((chars (string->list str)))
    (let build ((remaining chars) (freqs '()))
      (if (null? remaining)
          freqs
          (let* ((ch (car remaining))
                 (entry (assv ch freqs)))
            (if entry
                (build (cdr remaining)
                       (map (lambda (f)
                              (if (char=? (car f) ch)
                                  (cons ch (+ (cdr f) 1))
                                  f))
                            freqs))
                (build (cdr remaining)
                       (cons (cons ch 1) freqs))))))))

(define (sort-by-freq lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let* ((pivot (car lst))
             (rest (cdr lst))
             (lo (filter (lambda (x) (<= (cdr x) (cdr pivot))) rest))
             (hi (filter (lambda (x) (> (cdr x) (cdr pivot))) rest)))
        (append (sort-by-freq lo) (list pivot) (sort-by-freq hi)))))

;; --- Tree Construction ---
;; Insert node into sorted list (by weight), then merge two smallest.

(define (insert-sorted node queue)
  (cond
    ((null? queue) (list node))
    ((<= (node-weight* node) (node-weight* (car queue)))
     (cons node queue))
    (else (cons (car queue) (insert-sorted node (cdr queue))))))

(define (build-tree freqs)
  (if (null? freqs)
      (make-leaf #\space 0)
      (let ((queue (map (lambda (f) (make-leaf (car f) (cdr f)))
                        (sort-by-freq freqs))))
        (if (null? (cdr queue))
            (car queue)
            (let merge ((q queue))
              (if (null? (cdr q))
                  (car q)
                  (let* ((a (car q))
                         (b (cadr q))
                         (rest (cddr q))
                         (merged (make-branch a b
                                              (+ (node-weight* a)
                                                 (node-weight* b)))))
                    (merge (insert-sorted merged rest)))))))))

;; --- Code Table ---
;; Generate an alist of (char . "010...") prefix codes.

(define (generate-codes tree)
  (if (leaf? tree)
      (list (cons (leaf-char tree) "0"))
      (let ((codes '()))
        (let traverse ((node tree) (prefix '()))
          (if (leaf? node)
              (set! codes (cons (cons (leaf-char node)
                                      (list->string
                                       (map (lambda (b)
                                              (if (= b 0) #\0 #\1))
                                            (reverse prefix))))
                                codes))
              (begin
                (traverse (branch-left node) (cons 0 prefix))
                (traverse (branch-right node) (cons 1 prefix)))))
        codes)))

(define (lookup-code codes ch)
  (let ((entry (assv ch codes)))
    (if entry (cdr entry)
        (error "Character not in code table" ch))))

;; --- Encode / Decode ---

(define (encode str codes)
  (let ((chars (string->list str)))
    (apply string-append
           (map (lambda (ch) (lookup-code codes ch)) chars))))

(define (decode bits tree)
  (if (leaf? tree)
      (make-string (string-length bits) (leaf-char tree))
      (let walk ((i 0) (node tree) (result '()))
        (cond
          ((= i (string-length bits))
           (if (leaf? node)
               (list->string (reverse (cons (leaf-char node) result)))
               (list->string (reverse result))))
          ((leaf? node)
           (walk i tree (cons (leaf-char node) result)))
          ((char=? (string-ref bits i) #\0)
           (walk (+ i 1) (branch-left node) result))
          (else
           (walk (+ i 1) (branch-right node) result))))))

;; --- Display Helpers ---

(define (display-freq-table freqs)
  (let ((sorted (sort-by-freq freqs)))
    (display "  Char  Freq") (newline)
    (display "  ----  ----") (newline)
    (for-each
     (lambda (f)
       (display "  ")
       (cond
         ((char=? (car f) #\space) (display "' '"))
         ((char=? (car f) #\newline) (display "'\\n'"))
         (else (display " '") (display (car f)) (display "'")))
       (display "     ") (display (cdr f)) (newline))
     sorted)))

(define (display-code-table codes)
  (let ((sorted (sort (lambda (a b)
                        (< (string-length (cdr a))
                           (string-length (cdr b))))
                      codes)))
    (display "  Char  Code") (newline)
    (display "  ----  ----") (newline)
    (for-each
     (lambda (entry)
       (display "  ")
       (cond
         ((char=? (car entry) #\space) (display "' '"))
         ((char=? (car entry) #\newline) (display "'\\n'"))
         (else (display " '") (display (car entry)) (display "'")))
       (display "   ") (display (cdr entry)) (newline))
     sorted)))

(define (sort pred lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let* ((pivot (car lst))
             (rest (cdr lst))
             (lo (filter (lambda (x) (pred x pivot)) rest))
             (hi (filter (lambda (x) (not (pred x pivot))) rest)))
        (append (sort pred lo) (list pivot) (sort pred hi)))))

(define (display-tree tree indent)
  (cond
    ((leaf? tree)
     (display indent)
     (cond
       ((char=? (leaf-char tree) #\space) (display "' '"))
       ((char=? (leaf-char tree) #\newline) (display "'\\n'"))
       (else (display "'") (display (leaf-char tree)) (display "'")))
     (display " (") (display (node-weight tree)) (display ")")
     (newline))
    (else
     (display indent)
     (display "[") (display (branch-weight tree)) (display "]")
     (newline)
     (display-tree (branch-left tree)
                   (string-append indent "  0: "))
     (display-tree (branch-right tree)
                   (string-append indent "  1: ")))))

(define (compression-stats str encoded)
  (let* ((original-bits (* (string-length str) 8))
         (encoded-bits (string-length encoded))
         (ratio (if (zero? original-bits) 0
                    (inexact (* 100 (/ encoded-bits original-bits))))))
    (display "  Original:   ") (display original-bits) (display " bits (")
    (display (string-length str)) (display " chars x 8)") (newline)
    (display "  Encoded:    ") (display encoded-bits) (display " bits")
    (newline)
    (display "  Ratio:      ") (display ratio) (display "%")
    (newline)))

;; --- Demo ---

(define (run-full-example label str)
  (display "  ") (display label) (newline)
  (display "  Input: \"") (display str) (display "\"") (newline)
  (newline)

  (let* ((freqs (count-frequencies str))
         (tree (build-tree freqs))
         (codes (generate-codes tree))
         (encoded (encode str codes))
         (decoded (decode encoded tree)))

    (display-freq-table freqs)
    (newline)

    (display "  Tree:") (newline)
    (display-tree tree "    ")
    (newline)

    (display-code-table codes)
    (newline)

    (display "  Encoded: ") (display encoded) (newline)
    (display "  Decoded: \"") (display decoded) (display "\"") (newline)
    (display "  Match?   ") (display (equal? str decoded)) (newline)
    (newline)

    (compression-stats str encoded)
    (newline)))

(define (run-demo)
  (display "--- Huffman Coding Demo ---") (newline)
  (display "--- Optimal Prefix Codes from Character Frequencies ---")
  (newline) (newline)

  (run-full-example "1. Classic example" "ABRACADABRA")

  (display "  ---") (newline) (newline)

  (run-full-example "2. More variety"
    "the quick brown fox jumps over the lazy dog")

  (display "  ---") (newline) (newline)

  ;; Edge cases
  (display "  3. Edge cases") (newline) (newline)

  (let* ((str "AAAAAAA")
         (freqs (count-frequencies str))
         (tree (build-tree freqs))
         (codes (generate-codes tree))
         (encoded (encode str codes)))
    (display "  Single char: \"") (display str) (display "\"")
    (display " => ") (display encoded)
    (display " (") (display (string-length encoded)) (display " bits)")
    (newline))

  (let* ((str "ABABAB")
         (freqs (count-frequencies str))
         (tree (build-tree freqs))
         (codes (generate-codes tree))
         (encoded (encode str codes)))
    (display "  Two chars:   \"") (display str) (display "\"")
    (display " => ") (display encoded)
    (display " (") (display (string-length encoded)) (display " bits)")
    (newline))
  (newline)

  ;; Comparison
  (display "  4. Fixed-width vs Huffman comparison") (newline)
  (let* ((str "ABRACADABRA")
         (freqs (count-frequencies str))
         (n-chars (length freqs))
         (fixed-bits (let find-bits ((b 1))
                       (if (>= (expt 2 b) n-chars) b
                           (find-bits (+ b 1)))))
         (fixed-total (* fixed-bits (string-length str)))
         (tree (build-tree freqs))
         (codes (generate-codes tree))
         (huffman-total (string-length (encode str codes))))
    (display "  Characters:   ") (display n-chars) (display " unique")
    (newline)
    (display "  Fixed-width:  ") (display fixed-bits)
    (display " bits/char = ") (display fixed-total) (display " bits total")
    (newline)
    (display "  Huffman:      ") (display huffman-total)
    (display " bits total") (newline)
    (display "  Savings:      ")
    (display (- fixed-total huffman-total)) (display " bits (")
    (display (inexact (* 100 (/ (- fixed-total huffman-total) fixed-total))))
    (display "% reduction)") (newline)))

;; --- Analyze ---

(define (run-analyze str)
  (let* ((freqs (count-frequencies str))
         (tree (build-tree freqs))
         (codes (generate-codes tree)))
    (display "Analyzing: \"") (display str) (display "\"") (newline)
    (display "Length: ") (display (string-length str)) (display " chars")
    (newline) (newline)
    (display-freq-table freqs)
    (newline)
    (display-code-table codes)
    (newline)
    (let ((encoded (encode str codes)))
      (compression-stats str encoded))))

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
        (display "  demo              Run Huffman coding demonstrations") (newline)
        (display "  encode TEXT       Encode text and show full analysis") (newline)
        (display "  analyze TEXT      Show frequency table and codes") (newline))
      (let ((cmd (car args)))
        (cond
          ((equal? cmd "demo")
           (run-demo))

          ((equal? cmd "encode")
           (if (< (length args) 2)
               (begin
                 (display "Usage: kaappi app.scm encode TEXT") (newline)
                 (display "Example: kaappi app.scm encode \"ABRACADABRA\"")
                 (newline))
               (let* ((str (cadr args))
                      (freqs (count-frequencies str))
                      (tree (build-tree freqs))
                      (codes (generate-codes tree))
                      (encoded (encode str codes))
                      (decoded (decode encoded tree)))
                 (display "Input:   \"") (display str) (display "\"")
                 (newline) (newline)
                 (display-freq-table freqs)
                 (newline)
                 (display "Tree:") (newline)
                 (display-tree tree "  ")
                 (newline)
                 (display-code-table codes)
                 (newline)
                 (display "Encoded: ") (display encoded) (newline)
                 (display "Decoded: \"") (display decoded) (display "\"")
                 (newline)
                 (display "Match?   ") (display (equal? str decoded))
                 (newline) (newline)
                 (compression-stats str encoded))))

          ((equal? cmd "analyze")
           (if (< (length args) 2)
               (begin
                 (display "Usage: kaappi app.scm analyze TEXT") (newline))
               (run-analyze (cadr args))))

          (else
           (display "Unknown command: ") (display cmd) (newline)
           (display "Commands: demo, encode, analyze") (newline))))))
