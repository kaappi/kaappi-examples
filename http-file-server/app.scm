;;; HTTP File Server
;;;
;;; Serves static files from a directory, falling back to index.html
;;; for directory paths. Demonstrates HTTP serving with MIME type
;;; detection and HTML error pages.
;;;
;;; Usage: kaappi app.scm [port] [directory]
;;;   Default: port 8080, current directory

(import (scheme base) (scheme write)
        (scheme file) (scheme process-context)
        (srfi 13) (srfi 170) (kaappi http))

(define (user-args)
  (let ((args (command-line)))
    (cond
      ((null? args) '())
      ((string-suffix? ".scm" (car args)) (cdr args))
      ((and (pair? (cdr args)) (string-suffix? ".scm" (cadr args)))
       (cddr args))
      (else (cdr args)))))

(define cli-args (user-args))

(define serve-port
  (if (pair? cli-args)
      (or (string->number (car cli-args)) 8080)
      8080))

(define serve-dir
  (if (and (pair? cli-args) (pair? (cdr cli-args)))
      (cadr cli-args)
      "."))

;; --- MIME types ---

(define (guess-mime-type path)
  (cond
    ((string-suffix? ".html" path) "text/html")
    ((string-suffix? ".css"  path) "text/css")
    ((string-suffix? ".js"   path) "application/javascript")
    ((string-suffix? ".json" path) "application/json")
    ((string-suffix? ".txt"  path) "text/plain")
    ((string-suffix? ".scm"  path) "text/plain")
    ((string-suffix? ".sld"  path) "text/plain")
    ((string-suffix? ".md"   path) "text/markdown")
    ((string-suffix? ".xml"  path) "application/xml")
    ((string-suffix? ".csv"  path) "text/csv")
    (else "application/octet-stream")))

;; --- File reading ---

(define (read-text-file path)
  (and (file-exists? path)
       (call-with-input-file path
         (lambda (port)
           (let ((out (open-output-string)))
             (let loop ()
               (let ((ch (read-char port)))
                 (if (eof-object? ch)
                     (get-output-string out)
                     (begin (write-char ch out)
                            (loop))))))))))

;; --- Path safety ---

(define (safe-path? path)
  (not (string-contains path "..")))

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

;; The path if it names a regular file, #f otherwise. Directories fall
;; through to the index.html clause below.
(define (existing-file path)
  (and (file-exists? path)
       (file-info-regular? (file-info path))
       path))

(define (serve-file path)
  (let ((content (read-text-file path)))
    (if content
        (make-response 200 content
          (list (cons "Content-Type" (guess-mime-type path))))
        (make-response 500 "Could not read file"))))

(define (handler request)
  (let* ((url-path (request-path request))
         (file-path (string-append serve-dir url-path)))

    (display (request-method request)) (display " ") (display url-path) (newline)

    (cond
      ;; Path traversal protection
      ((not (safe-path? url-path))
       (make-response 403 "Forbidden"))

      ;; Serve the file itself, or index.html for a directory
      ((existing-file file-path) => serve-file)
      ((existing-file (string-append file-path "/index.html")) => serve-file)

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
