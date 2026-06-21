;;; REST API — User management with PostgreSQL + Redis cache
;;;
;;; Endpoints:
;;;   GET    /users          — list all users
;;;   GET    /users/:id      — get user by id (cached in Redis)
;;;   POST   /users          — create user (body: name=...&email=...)
;;;   DELETE /users/:id      — delete user (invalidates cache)
;;;   GET    /health         — health check
;;;
;;; Requires: PostgreSQL (createdb kaappi_demo), Redis

(import (scheme base) (scheme write) (scheme cxr)
        (kaappi http) (kaappi pg) (kaappi redis))

;; --- Configuration ---

(define pg-conninfo "dbname=kaappi_demo")
(define redis-host "127.0.0.1")
(define redis-port 6379)
(define http-port 8080)

;; --- Database setup ---

(define db (pg-connect pg-conninfo))
(define cache (redis-connect redis-host redis-port))

(pg-exec db "CREATE TABLE IF NOT EXISTS users (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT NOT NULL,
  created_at TIMESTAMP DEFAULT now()
)")

(display "Database ready") (newline)

;; --- Helpers ---

(define (user->json row)
  (string-append
    "{\"id\":" (number->string (vector-ref row 0))
    ",\"name\":\"" (vector-ref row 1) "\""
    ",\"email\":\"" (vector-ref row 2) "\""
    ",\"created_at\":\"" (vector-ref row 3) "\"}"))

(define (users->json rows)
  (string-append
    "[" (let loop ((rs rows) (first #t))
          (if (null? rs)
              "]"
              (string-append
                (if first "" ",")
                (user->json (car rs))
                (loop (cdr rs) #f))))))

(define (json-headers)
  '(("Content-Type" . "application/json")))

(define (parse-path-id path prefix)
  (if (and (> (string-length path) (string-length prefix))
           (equal? (substring path 0 (string-length prefix)) prefix))
      (string->number (substring path (string-length prefix)
                                 (string-length path)))
      #f))

(define (parse-form-body body)
  (if (or (not body) (equal? body ""))
      '()
      (map (lambda (pair-str)
             (let loop ((i 0))
               (cond ((= i (string-length pair-str))
                      (cons pair-str ""))
                     ((char=? (string-ref pair-str i) #\=)
                      (cons (substring pair-str 0 i)
                            (substring pair-str (+ i 1) (string-length pair-str))))
                     (else (loop (+ i 1))))))
           (let split ((s body) (acc '()))
             (let loop ((i 0))
               (cond ((= i (string-length s))
                      (reverse (cons s acc)))
                     ((char=? (string-ref s i) #\&)
                      (split (substring s (+ i 1) (string-length s))
                             (cons (substring s 0 i) acc)))
                     (else (loop (+ i 1)))))))))

;; --- Cache helpers ---

(define (cache-key id)
  (string-append "user:" (number->string id)))

(define (get-user-cached id)
  (let ((cached (redis-get cache (cache-key id))))
    (if cached
        (begin
          (display "  [cache hit] user ") (display id) (newline)
          cached)
        (let ((rows (pg-query db
                      "SELECT id, name, email, created_at::text FROM users WHERE id = $1"
                      id)))
          (if (null? rows)
              #f
              (let ((json (user->json (car rows))))
                (redis-set cache (cache-key id) json)
                (redis-expire cache (cache-key id) 60)
                (display "  [cache miss] user ") (display id) (newline)
                json))))))

;; --- Route handler ---

(define (handler request)
  (let ((method (request-method request))
        (path   (request-path request)))

    (display method) (display " ") (display path) (newline)

    (cond
      ;; Health check
      ((equal? path "/health")
       (make-response 200 "{\"status\":\"ok\"}" (json-headers)))

      ;; List users
      ((and (equal? method "GET") (equal? path "/users"))
       (let ((rows (pg-query db
                     "SELECT id, name, email, created_at::text FROM users ORDER BY id")))
         (make-response 200 (users->json rows) (json-headers))))

      ;; Get user by id
      ((and (equal? method "GET") (parse-path-id path "/users/"))
       => (lambda (id)
            (let ((json (get-user-cached id)))
              (if json
                  (make-response 200 json (json-headers))
                  (make-response 404
                    "{\"error\":\"User not found\"}" (json-headers))))))

      ;; Create user
      ((and (equal? method "POST") (equal? path "/users"))
       (let* ((params (parse-form-body (request-body request)))
              (name  (cdr (or (assoc "name" params) '("" . ""))))
              (email (cdr (or (assoc "email" params) '("" . "")))))
         (if (or (equal? name "") (equal? email ""))
             (make-response 400
               "{\"error\":\"name and email required\"}" (json-headers))
             (let ((rows (pg-query db
                           "INSERT INTO users (name, email) VALUES ($1, $2)
                            RETURNING id, name, email, created_at::text"
                           name email)))
               (make-response 201 (user->json (car rows)) (json-headers))))))

      ;; Delete user
      ((and (equal? method "DELETE") (parse-path-id path "/users/"))
       => (lambda (id)
            (let ((n (pg-exec db "DELETE FROM users WHERE id = $1" id)))
              (redis-del cache (cache-key id))
              (if (> n 0)
                  (make-response 200
                    "{\"deleted\":true}" (json-headers))
                  (make-response 404
                    "{\"error\":\"User not found\"}" (json-headers))))))

      ;; 404
      (else
       (make-response 404
         "{\"error\":\"Not found\"}" (json-headers))))))

;; --- Start ---

(display (string-append "Starting REST API on port "
                        (number->string http-port) "..."))
(newline)
(http-listen handler http-port)
