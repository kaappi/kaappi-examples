;;; Redis Task Queue
;;;
;;; Demonstrates using Redis as a simple job queue with producer/consumer pattern.
;;; Run two instances: one as producer, one as worker.
;;;
;;;   kaappi app.scm producer     — enqueue tasks
;;;   kaappi app.scm worker       — process tasks

(import (scheme base) (scheme write) (scheme process-context)
        (srfi 13) (kaappi redis))

(define conn (redis-connect "127.0.0.1" 6379))
(define queue-key "task-queue")
(define results-key "task-results")

;; --- Producer ---

(define (produce-tasks)
  (display "=== Task Producer ===") (newline)
  (let loop ((i 1))
    (when (<= i 10)
      (let ((task (string-append "{\"id\":" (number->string i)
                                 ",\"action\":\"compute\""
                                 ",\"value\":" (number->string (* i i)) "}")))
        (redis-lpush conn queue-key task)
        (display "  Enqueued task ") (display i) (newline))
      (loop (+ i 1))))
  (display "Done. 10 tasks enqueued.") (newline)
  (display "Queue length: ") (display (redis-llen conn queue-key)) (newline))

;; --- Worker ---

(define (process-task task-json)
  (display "  Processing: ") (display task-json) (newline)
  (string-append "{\"status\":\"done\",\"input\":" task-json "}"))

(define (run-worker)
  (display "=== Task Worker ===") (newline)
  (display "Waiting for tasks...") (newline)
  (let loop ()
    (let ((task (redis-rpop conn queue-key)))
      (if task
          (begin
            (redis-lpush conn results-key (process-task task))
            (loop))
          (begin
            (display "No more tasks. Worker done.") (newline))))))

;; --- Status ---

(define (show-status)
  (display "=== Queue Status ===") (newline)
  (display "Pending tasks: ") (display (redis-llen conn queue-key)) (newline)
  (display "Results: ") (display (redis-llen conn results-key)) (newline)
  (let ((results (redis-lrange conn results-key 0 -1)))
    (for-each (lambda (r) (display "  ") (display r) (newline)) results)))

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
  (display "Usage: kaappi app.scm [producer|worker|status]") (newline)
  (display "  producer — enqueue 10 sample tasks") (newline)
  (display "  worker   — process all pending tasks") (newline)
  (display "  status   — show queue and results") (newline))

(let ((args (user-args)))
  (if (null? args)
      (show-usage)
      (let ((cmd (car args)))
        (cond
          ((equal? cmd "producer") (produce-tasks))
          ((equal? cmd "worker")   (run-worker))
          ((equal? cmd "status")   (show-status))
          (else (show-usage))))))

(redis-disconnect! conn)
