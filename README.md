# kaappi-examples

Example applications for [Kaappi Scheme](https://github.com/kaappi/kaappi),
showcasing pure Scheme programming and the ecosystem libraries including
[Redis](https://github.com/kaappi/kaappi-redis),
[PostgreSQL](https://github.com/kaappi/kaappi-pg),
[HTTP](https://github.com/kaappi/kaappi-http),
[JSON](https://github.com/kaappi/kaappi-json), and
[Web](https://github.com/kaappi/kaappi-web).

## Setup

Install all libraries with [thottam](https://github.com/kaappi/kaappi):

```bash
# One-time setup
thottam install kaappi-web     # installs kaappi-http, kaappi-json, kaappi-net
thottam install kaappi-redis
thottam install kaappi-pg

# Verify
thottam list
```

> If thottam is not on your PATH, run it directly: `../kaappi/scripts/thottam install ...`

## Examples

### REST API (Redis + PostgreSQL + HTTP + JSON)

A full REST API server with PostgreSQL storage, Redis caching, and
JSON request/response handling via the kaappi-web framework.

```bash
createdb kaappi_demo
redis-server --daemonize yes
cd rest-api && kaappi app.scm

# In another terminal:
curl -X POST -H "Content-Type: application/json" \
     -d '{"name":"Alice","email":"alice@example.com"}' \
     http://localhost:8080/users
curl http://localhost:8080/users
curl http://localhost:8080/users/1    # cached in Redis
```

See [rest-api/README.md](rest-api/README.md) for full documentation.

### Redis Task Queue

Producer/consumer job queue using Redis lists.

```bash
cd redis-task-queue
kaappi app.scm producer   # enqueue 10 tasks
kaappi app.scm worker     # process all tasks
kaappi app.scm status     # show results
```

### PostgreSQL CRUD

Interactive contact book with full CRUD, search, and statistics.

```bash
cd pg-crud
createdb kaappi_demo
kaappi app.scm seed                              # insert sample data
kaappi app.scm list                              # list all contacts
kaappi app.scm search alice                      # search by name/email
kaappi app.scm add "Eve" "eve@test.com" "555-0"  # add contact
kaappi app.scm stats                             # show statistics
```

### Symbolic Differentiation (Pure Scheme)

Constructs ASTs for mathematical expressions and computes symbolic
derivatives using recursive tree-walking with algebraic simplification.
No external libraries needed.

```bash
cd symbolic-differentiation
kaappi app.scm demo                            # show example derivatives
kaappi app.scm diff "(* x x)" x                # d/dx(x*x) = 2*x
kaappi app.scm diff "(sin (* 2 x))" x          # chain rule
kaappi app.scm diff "(ln (+ (^ x 2) 1))" x    # 2x / (x^2 + 1)
kaappi app.scm simplify "(+ (* 1 x) (* y 0))"  # simplify to x
kaappi app.scm eval "(+ (* 2 3) (- 10 4))"     # evaluate to 12
```

### Metacircular Evaluator (Pure Scheme)

A self-interpreting Scheme interpreter — Scheme code that evaluates
Scheme code. Implements lexical scoping, closures, mutation, and the
classic eval/apply loop. No external libraries needed.

```bash
cd metacircular-evaluator
kaappi app.scm demo                                         # run all demos
kaappi app.scm eval "(+ 1 2)"                               # evaluate expression
kaappi app.scm eval "(define (f x) (* x x)) (f 5)"          # multi-expression
kaappi app.scm eval "(define (adder n) (lambda (x) (+ n x))) ((adder 10) 32)"
kaappi app.scm repl                                         # interactive REPL
```

### HTTP File Server

Serves static files from a directory with MIME type detection.

```bash
cd http-file-server
kaappi app.scm 8080 .     # serve current directory on port 8080
# Then: open http://localhost:8080/app.scm
```
