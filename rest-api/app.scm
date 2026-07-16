;;; REST API — User management with PostgreSQL + Redis cache
;;;
;;; Endpoints:
;;;   GET    /users          — list all users
;;;   GET    /users/:id      — get user by id (cached in Redis)
;;;   POST   /users          — create user (JSON body or form body)
;;;   DELETE /users/:id      — delete user (invalidates cache)
;;;   GET    /health         — health check
;;;
;;; Requires: PostgreSQL (createdb kaappi_demo), Redis

(import (scheme base) (scheme write) (srfi 13)
        (kaappi http) (kaappi pg) (kaappi redis) (kaappi json))

;; --- Configuration ---

(define pg-conninfo "dbname=kaappi_demo")
(define redis-host "127.0.0.1")
(define redis-port 6379)
(define http-port 8080)

;; --- Database setup ---

;; Connect and make sure the schema exists. Binding the connection with
;; define keeps script mode from echoing pg-exec's return value.
(define db
  (let ((conn (pg-connect pg-conninfo)))
    (pg-exec conn "CREATE TABLE IF NOT EXISTS users (
      id SERIAL PRIMARY KEY,
      name TEXT NOT NULL,
      email TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT now()
    )")
    conn))

(define cache (redis-connect redis-host redis-port))

(display "Database ready") (newline)

;; --- Helpers ---

(define json-headers '(("Content-Type" . "application/json")))

(define (json-response status body)
  (make-response status (json-write-string body) json-headers))

(define (row->user row)
  `(("id"         . ,(vector-ref row 0))
    ("name"       . ,(vector-ref row 1))
    ("email"      . ,(vector-ref row 2))
    ("created_at" . ,(vector-ref row 3))))

(define (parse-path-id path prefix)
  (and (string-prefix? prefix path)
       (string->number (substring path (string-length prefix)
                                  (string-length path)))))

;; "name=Alice" -> ("name" . "Alice"), split on the first #\=
(define (split-form-pair pair-str)
  (let ((i (string-index pair-str #\=)))
    (if i
        (cons (substring pair-str 0 i)
              (substring pair-str (+ i 1) (string-length pair-str)))
        (cons pair-str ""))))

(define (parse-body request)
  (let ((body (request-body request))
        (ct   (or (request-header request "content-type") "")))
    (cond
      ((equal? body "") '())
      ;; JSON body: {"name": "Alice", "email": "alice@example.com"}
      ((string-contains ct "application/json")
       (let ((obj (json-read-string body)))
         (if (list? obj) obj '())))
      ;; Form body: name=Alice&email=alice@example.com
      (else
       (map split-form-pair (string-split body "&"))))))

;; Look up a form/JSON parameter, defaulting to "".
(define (param-ref params key)
  (cond ((assoc key params) => cdr)
        (else "")))

;; --- Cache helpers ---

(define (cache-key id)
  (string-append "user:" (number->string id)))

(define (get-user-cached id)
  (let ((cached (redis-get cache (cache-key id))))
    (if cached
        (begin
          (display "  [cache hit] user ") (display id) (newline)
          (json-read-string cached))
        (let ((rows (pg-query db
                      "SELECT id, name, email, created_at::text FROM users WHERE id = $1"
                      id)))
          (if (null? rows)
              #f
              (let ((user (row->user (car rows))))
                (redis-set cache (cache-key id) (json-write-string user))
                (redis-expire cache (cache-key id) 60)
                (display "  [cache miss] user ") (display id) (newline)
                user))))))

;; --- Route handler ---

(define (handler request)
  (let ((method (request-method request))
        (path   (request-path request)))

    (display method) (display " ") (display path) (newline)

    (cond
      ;; Health check
      ((equal? path "/health")
       (json-response 200 '(("status" . "ok"))))

      ;; List users
      ((and (equal? method "GET") (equal? path "/users"))
       (let ((rows (pg-query db
                     "SELECT id, name, email, created_at::text FROM users ORDER BY id")))
         (json-response 200 (map row->user rows))))

      ;; Get user by id
      ((and (equal? method "GET") (parse-path-id path "/users/"))
       => (lambda (id)
            (let ((user (get-user-cached id)))
              (if user
                  (json-response 200 user)
                  (json-response 404 '(("error" . "User not found")))))))

      ;; Create user
      ((and (equal? method "POST") (equal? path "/users"))
       (let* ((params (parse-body request))
              (name  (param-ref params "name"))
              (email (param-ref params "email")))
         (if (or (equal? name "") (equal? email ""))
             (json-response 400 '(("error" . "name and email required")))
             (let ((rows (pg-query db
                           "INSERT INTO users (name, email) VALUES ($1, $2)
                            RETURNING id, name, email, created_at::text"
                           name email)))
               (json-response 201 (row->user (car rows)))))))

      ;; Delete user
      ((and (equal? method "DELETE") (parse-path-id path "/users/"))
       => (lambda (id)
            (let ((n (pg-exec db "DELETE FROM users WHERE id = $1" id)))
              (redis-del cache (cache-key id))
              (if (> n 0)
                  (json-response 200 '(("deleted" . #t)))
                  (json-response 404 '(("error" . "User not found")))))))

      ;; 404
      (else
       (json-response 404 '(("error" . "Not found")))))))

;; --- Start ---

(display "Starting REST API on port ")
(display http-port) (display "...") (newline)
(http-listen handler http-port)
