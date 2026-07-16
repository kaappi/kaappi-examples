;;; PostgreSQL CRUD — Interactive contact book
;;;
;;; Demonstrates the DB-API 2.0 style with cursors, transactions,
;;; parameterized queries, and type conversion.
;;;
;;; Usage: kaappi app.scm [add|list|search|delete|stats]

(import (scheme base) (scheme write) (scheme process-context)
        (srfi 13) (kaappi pg))

;; Connect and make sure the schema exists. Binding the connection with
;; define keeps script mode from echoing pg-exec's return value.
(define db
  (let ((conn (pg-connect "dbname=kaappi_demo")))
    (pg-exec conn "CREATE TABLE IF NOT EXISTS contacts (
      id SERIAL PRIMARY KEY,
      name TEXT NOT NULL,
      email TEXT,
      phone TEXT,
      favorite BOOLEAN DEFAULT FALSE,
      created_at TIMESTAMP DEFAULT now()
    )")
    conn))

;; --- Commands ---

(define (add-contact name email phone)
  (let* ((rows (pg-query db
                 "INSERT INTO contacts (name, email, phone)
                  VALUES ($1, $2, $3)
                  RETURNING id, name, email, phone"
                 name
                 (if (equal? email "") #f email)
                 (if (equal? phone "") #f phone)))
         (row (car rows)))
    (display "Created contact #") (display (vector-ref row 0))
    (display ": ") (display (vector-ref row 1))
    (newline)))

(define (list-contacts)
  (let ((cur (pg-cursor db)))
    (pg-execute cur
      "SELECT id, name, email, phone, favorite FROM contacts ORDER BY name")
    (display (pg-rowcount cur)) (display " contacts:") (newline)
    (newline)
    (let loop ()
      (let ((row (pg-fetchone cur)))
        (when row
          (display "  #") (display (vector-ref row 0))
          (display "  ") (display (vector-ref row 1))
          (when (vector-ref row 2)
            (display "  <") (display (vector-ref row 2)) (display ">"))
          (when (vector-ref row 3)
            (display "  ") (display (vector-ref row 3)))
          (when (vector-ref row 4)
            (display "  *"))
          (newline)
          (loop))))
    (pg-cursor-close cur)))

(define (search-contacts term)
  (let ((pattern (string-append "%" term "%")))
    (let ((rows (pg-query db
                  "SELECT id, name, email, phone FROM contacts
                   WHERE name ILIKE $1 OR email ILIKE $1 OR phone ILIKE $1
                   ORDER BY name"
                  pattern)))
      (display (length rows)) (display " matches:") (newline)
      (for-each
        (lambda (row)
          (display "  #") (display (vector-ref row 0))
          (display "  ") (display (vector-ref row 1))
          (when (vector-ref row 2)
            (display "  <") (display (vector-ref row 2)) (display ">"))
          (newline))
        rows))))

(define (delete-contact id-str)
  (let ((id (string->number id-str)))
    (cond
      ((not id)
       (display "Invalid ID") (newline))
      ((> (pg-exec db "DELETE FROM contacts WHERE id = $1" id) 0)
       (display "Deleted contact #") (display id) (newline))
      (else
       (display "Contact not found") (newline)))))

(define (show-stats)
  (let* ((rows (pg-query db
                 "SELECT
                    COUNT(*) AS total,
                    COUNT(email) AS with_email,
                    COUNT(phone) AS with_phone,
                    COUNT(*) FILTER (WHERE favorite) AS favorites
                  FROM contacts"))
         (row (car rows)))
    (display "Contact Statistics:") (newline)
    (display "  Total:     ") (display (vector-ref row 0)) (newline)
    (display "  Has email: ") (display (vector-ref row 1)) (newline)
    (display "  Has phone: ") (display (vector-ref row 2)) (newline)
    (display "  Favorites: ") (display (vector-ref row 3)) (newline)))

(define (seed-data)
  (call-with-pg-transaction db
    (lambda ()
      (pg-exec db "DELETE FROM contacts")
      (pg-exec db "INSERT INTO contacts (name, email, phone, favorite) VALUES
        ($1, $2, $3, $4)" "Alice Smith" "alice@example.com" "555-0101" #t)
      (pg-exec db "INSERT INTO contacts (name, email, phone) VALUES
        ($1, $2, $3)" "Bob Johnson" "bob@example.com" "555-0102")
      (pg-exec db "INSERT INTO contacts (name, email) VALUES
        ($1, $2)" "Charlie Brown" "charlie@example.com")
      (pg-exec db "INSERT INTO contacts (name, phone) VALUES
        ($1, $2)" "Diana Prince" "555-0104")))
  (display "Seeded 4 contacts") (newline))

;; --- Main ---

(define (user-args)
  (let ((args (command-line)))
    (cond
      ((null? args) '())
      ((string-suffix? ".scm" (car args)) (cdr args))
      ((and (pair? (cdr args)) (string-suffix? ".scm" (cadr args)))
       (cddr args))
      (else (cdr args)))))

(define (show-usage)
  (display "Usage: kaappi app.scm <command>") (newline)
  (display "  seed                      — insert sample data") (newline)
  (display "  list                      — list all contacts") (newline)
  (display "  search <term>             — search by name/email/phone") (newline)
  (display "  add <name> <email> <phone> — add a contact") (newline)
  (display "  delete <id>               — delete by ID") (newline)
  (display "  stats                     — show statistics") (newline))

(let ((args (user-args)))
  (if (null? args)
      (show-usage)
      (let ((cmd (car args)))
        (cond
          ((and (equal? cmd "add") (>= (length args) 4))
           (add-contact (cadr args) (caddr args) (cadddr args)))
          ((equal? cmd "list") (list-contacts))
          ((and (equal? cmd "search") (>= (length args) 2))
           (search-contacts (cadr args)))
          ((and (equal? cmd "delete") (>= (length args) 2))
           (delete-contact (cadr args)))
          ((equal? cmd "stats") (show-stats))
          ((equal? cmd "seed") (seed-data))
          (else (show-usage))))))

(pg-close db)
