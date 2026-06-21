;;; HTTP File Server
;;;
;;; Serves static files from a directory with directory listing.
;;; Demonstrates HTTP server with dynamic content generation.
;;;
;;; Usage: kaappi app.scm [port] [directory]
;;;   Default: port 8080, current directory

(import (scheme base) (scheme write) (scheme read)
        (scheme file) (scheme process-context)
        (kaappi http))

(define serve-port 8080)
(define serve-dir ".")

(let ((args (command-line)))
  (when (>= (length args) 3)
    (set! serve-port (or (string->number (list-ref args 2)) 8080)))
  (when (>= (length args) 4)
    (set! serve-dir (list-ref args 3))))

;; --- MIME types ---

(define (guess-mime-type path)
  (cond
    ((has-suffix? path ".html") "text/html")
    ((has-suffix? path ".css")  "text/css")
    ((has-suffix? path ".js")   "application/javascript")
    ((has-suffix? path ".json") "application/json")
    ((has-suffix? path ".txt")  "text/plain")
    ((has-suffix? path ".scm")  "text/plain")
    ((has-suffix? path ".sld")  "text/plain")
    ((has-suffix? path ".md")   "text/markdown")
    ((has-suffix? path ".xml")  "application/xml")
    ((has-suffix? path ".csv")  "text/csv")
    (else "application/octet-stream")))

(define (has-suffix? str suffix)
  (and (>= (string-length str) (string-length suffix))
       (equal? (substring str (- (string-length str) (string-length suffix))
                          (string-length str))
               suffix)))

;; --- File reading ---

(define (read-text-file path)
  (if (file-exists? path)
      (let ((port (open-input-file path)))
        (let loop ((acc (open-output-string)))
          (let ((ch (read-char port)))
            (if (eof-object? ch)
                (begin (close-input-port port)
                       (get-output-string acc))
                (begin (write-char ch acc)
                       (loop acc))))))
      #f))

;; --- Path safety ---

(define (safe-path? path)
  (let ((len (string-length path)))
    (let loop ((i 0))
      (cond
        ((>= i (- len 1)) #t)
        ((and (char=? (string-ref path i) #\.)
              (char=? (string-ref path (+ i 1)) #\.))
         #f)
        (else (loop (+ i 1)))))))

;; --- HTML helpers ---

(define (html-escape s)
  (let ((out (open-output-string)))
    (let loop ((i 0))
      (when (< i (string-length s))
        (let ((ch (string-ref s i)))
          (cond
            ((char=? ch #\<) (display "&lt;" out))
            ((char=? ch #\>) (display "&gt;" out))
            ((char=? ch #\&) (display "&amp;" out))
            ((char=? ch #\") (display "&quot;" out))
            (else (write-char ch out))))
        (loop (+ i 1))))
    (get-output-string out)))

;; --- Handler ---

(define (handler request)
  (let* ((url-path (request-path request))
         (file-path (string-append serve-dir url-path)))

    (display (request-method request)) (display " ") (display url-path) (newline)

    (cond
      ;; Path traversal protection
      ((not (safe-path? url-path))
       (make-response 403 "Forbidden"))

      ;; Try to serve file
      ((file-exists? file-path)
       (let ((content (read-text-file file-path)))
         (if content
             (make-response 200 content
               (list (cons "Content-Type" (guess-mime-type file-path))))
             (make-response 500 "Could not read file"))))

      ;; Index file
      ((file-exists? (string-append file-path "/index.html"))
       (let ((content (read-text-file (string-append file-path "/index.html"))))
         (make-response 200 content
           '(("Content-Type" . "text/html")))))

      ;; 404
      (else
       (make-response 404
         (string-append "<html><body><h1>404 Not Found</h1><p>"
                        (html-escape url-path) " not found</p></body></html>")
         '(("Content-Type" . "text/html")))))))

;; --- Start ---

(display "Serving ") (display serve-dir)
(display " on port ") (display serve-port)
(newline)
(http-listen handler serve-port)
