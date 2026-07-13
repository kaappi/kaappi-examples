# kaappi-examples

[![CI](https://github.com/kaappi/kaappi-examples/actions/workflows/ci.yml/badge.svg)](https://github.com/kaappi/kaappi-examples/actions/workflows/ci.yml)
[![Kaappi v0.10.0](https://img.shields.io/badge/kaappi-v0.10.0-blue)](https://github.com/kaappi/kaappi)
[![License: MIT](https://img.shields.io/badge/license-MIT-green)](LICENSE)

Real-world example programs for [Kaappi Scheme](https://github.com/kaappi/kaappi) — from sorting algorithms and maze solvers to self-interpreting evaluators and REST APIs.

Every pure Scheme example runs with **zero dependencies** — install Kaappi and go:

```bash
kaappi sorting/app.scm demo
kaappi game-of-life/app.scm demo
kaappi metacircular-evaluator/app.scm demo
```

The ecosystem examples show Kaappi working with PostgreSQL, Redis, and HTTP to build production-style applications.

## Examples at a Glance

| Example | What it does | Difficulty | Key Concepts |
|---------|-------------|:----------:|--------------|
| [Sorting Algorithms](#sorting-algorithms) | Quicksort & merge sort, purely functional | Beginner | Higher-order functions, closures, comparators |
| [Game of Life](#game-of-life) | Conway's automaton on a toroidal grid | Beginner | Grid traversal, functional update, vectors |
| [Maze Solver](#maze-solver) | Generate & solve mazes with DFS backtracking | Intermediate | Recursion, immutable state, bitwise ops |
| [Huffman Coding](#huffman-coding) | Build prefix codes, encode/decode messages | Intermediate | Tree construction, priority queues, compression |
| [Matrix Math](#matrix-math) | Matrix multiply, power, exact rationals | Intermediate | Nested recursion, accumulators, exact arithmetic |
| [Parallel Prime Search](#parallel-prime-search) | Count primes across CPU cores with a worker pool | Intermediate | `(kaappi parallel)`, worker pools, chunking |
| [Symbolic Differentiation](#symbolic-differentiation) | Symbolic derivatives with simplification | Advanced | AST manipulation, pattern matching, algebra |
| [Metacircular Evaluator](#metacircular-evaluator) | Scheme interpreting Scheme | Advanced | eval/apply, lexical scoping, closures |
| [Environment-Model Evaluator](#environment-model-evaluator) | Eager vs. lazy evaluation with thunks | Advanced | Thunks, memoization, call-by-need |
| [HTTP File Server](#http-file-server) | Serve static files with MIME detection | Beginner | HTTP, file I/O |
| [PostgreSQL CRUD](#postgresql-crud) | Contact book with search & statistics | Intermediate | SQL, transactions, cursors |
| [Redis Task Queue](#redis-task-queue) | Producer/consumer job queue | Intermediate | Redis lists, message patterns |
| [REST API](#rest-api) | Full REST server with caching | Advanced | Web framework, JSON, PostgreSQL, Redis |

## Quick Start

```bash
# 1. Install Kaappi (or build from source — see Setup below)
#    Download a release binary from https://github.com/kaappi/kaappi/releases

# 2. Clone and run any pure Scheme example — no other setup needed
git clone https://github.com/kaappi/kaappi-examples.git
cd kaappi-examples
kaappi sorting/app.scm demo
```

---

## Pure Scheme Examples

These examples use only R7RS standard libraries. No external dependencies, no
build step — just `kaappi <example>/app.scm demo`.

### Sorting Algorithms

**Difficulty:** Beginner | **Concepts:** Higher-order functions, closures, function composition, stability

Quicksort and merge sort implemented purely with higher-order functions —
no mutation in the sorting algorithms. Includes from-scratch `filter`,
`fold`, `partition`, custom comparators, sort-by-key, and benchmarking.

```bash
cd sorting
kaappi app.scm demo                    # run all sorting demonstrations
kaappi app.scm bench 5000              # benchmark with 5000 random elements
kaappi app.scm quick 5 3 8 1 9         # quicksort numbers from CLI
kaappi app.scm merge banana apple date # merge sort strings from CLI
```

### Game of Life

**Difficulty:** Beginner | **Concepts:** Grid traversal, functional update, vectors, ANSI animation

Conway's cellular automaton on a toroidal grid. Each generation builds
a fresh grid from the previous one — the source grid is never mutated
during computation. Includes glider, blinker, Gosper glider gun,
R-pentomino, random grids, and animated terminal display.

```bash
cd game-of-life
kaappi app.scm demo                    # blinker, glider, gun, R-pentomino, random
kaappi app.scm glider 20 10 30         # glider on 20x10 grid, 30 generations
kaappi app.scm gun 40 20 100           # Gosper glider gun
kaappi app.scm random 30 20 50 42 30   # random 30x20, seed 42, 30% density
kaappi app.scm animate glider 20 10 50 # animated terminal display
```

### Maze Solver

**Difficulty:** Intermediate | **Concepts:** DFS backtracking, immutable visited sets, bitwise operations, ASCII rendering

Generates random mazes using the recursive backtracker algorithm and
solves them with DFS over immutable visited sets. Wall states encoded
as bitmasks. Renders ASCII art with solution path overlay.

```bash
cd maze-solver
kaappi app.scm demo                    # generate + solve 5x5, 10x10, 15x15
kaappi app.scm generate 20 10          # just display a 20x10 maze
kaappi app.scm solve 15 10             # generate + solve + show path
kaappi app.scm solve 15 10 42          # solve with specific RNG seed
```

### Huffman Coding

**Difficulty:** Intermediate | **Concepts:** Binary trees, priority queues, prefix codes, compression analysis

Builds optimal prefix codes from character frequencies. Encodes and
decodes messages with bit-level compression, showing tree structure,
code tables, and compression ratios vs. fixed-width encoding.

```bash
cd huffman-coding
kaappi app.scm demo                    # full demo with multiple examples
kaappi app.scm encode "MISSISSIPPI"    # encode with full analysis
kaappi app.scm analyze "hello world"   # frequency table and code lengths
```

### Matrix Math

**Difficulty:** Intermediate | **Concepts:** Nested recursion, explicit accumulators, exact rational arithmetic

Matrix operations using nested recursion with explicit accumulator
parameters — no `set!` in any computation. Exact rational arithmetic
preserved through all operations. Includes matrix exponentiation by
squaring for computing Fibonacci numbers via matrix power.

```bash
cd matrix-math
kaappi app.scm demo                                       # run all demos
kaappi app.scm multiply "((1 2) (3 4))" "((5 6) (7 8))"  # multiply two matrices
kaappi app.scm power "((1 1) (1 0))" 20                   # Fibonacci via M^20
```

### Parallel Prime Search

**Difficulty:** Intermediate | **Concepts:** `(kaappi parallel)` worker pools, chunking work across processors, sequential vs. parallel timing

Counts primes below N by trial division, splitting the range into one
chunk per available processor and running the chunks on a
`(kaappi parallel)` pool — real multi-core speedup, not just cooperative
fibers. `demo` compares sequential and parallel timing directly.

```bash
cd parallel-primes
kaappi app.scm demo           # sequential vs. parallel timing comparison
kaappi app.scm count 200000   # count primes below 200000
```

### Symbolic Differentiation

**Difficulty:** Advanced | **Concepts:** AST construction, recursive tree-walking, algebraic simplification, chain rule

Constructs ASTs for mathematical expressions and computes symbolic
derivatives. Supports `+`, `-`, `*`, `/`, `^`, `sin`, `cos`, `exp`,
`ln` with chain rule, product rule, and quotient rule. Includes an
algebraic simplifier and expression evaluator.

```bash
cd symbolic-differentiation
kaappi app.scm demo                            # show example derivatives
kaappi app.scm diff "(* x x)" x                # d/dx(x^2) = 2*x
kaappi app.scm diff "(sin (* 2 x))" x          # chain rule
kaappi app.scm diff "(ln (+ (^ x 2) 1))" x    # 2x / (x^2 + 1)
kaappi app.scm simplify "(+ (* 1 x) (* y 0))"  # simplify to x
kaappi app.scm eval "(+ (* 2 3) (- 10 4))"     # evaluate to 12
```

### Metacircular Evaluator

**Difficulty:** Advanced | **Concepts:** eval/apply loop, lexical scoping, closures, mutation, self-interpretation

A self-interpreting Scheme interpreter — Scheme code that evaluates
Scheme code. Implements the classic eval/apply loop with ribcage
environments, closures, `define`/`set!`, `let`/`let*`, `cond`,
`and`/`or`, and 35+ built-in primitives.

```bash
cd metacircular-evaluator
kaappi app.scm demo                                         # run all demos
kaappi app.scm eval "(+ 1 2)"                               # evaluate expression
kaappi app.scm eval "(define (f x) (* x x)) (f 5)"          # multi-expression
kaappi app.scm eval "(define (adder n) (lambda (x) (+ n x))) ((adder 10) 32)"
kaappi app.scm repl                                         # interactive REPL
```

### Environment-Model Evaluator

**Difficulty:** Advanced | **Concepts:** Thunks, memoization, call-by-need, lazy vs. eager evaluation, letrec

Extends the metacircular evaluator with switchable evaluation strategy.
In normal-order (lazy) mode, arguments are wrapped as thunks and forced
on demand with memoization. Demonstrates expressions that terminate
under lazy evaluation but diverge or error under eager evaluation.

```bash
cd env-evaluator
kaappi app.scm demo                                         # all demos (eager + lazy)
kaappi app.scm eval "(+ 1 2)"                               # evaluate eagerly
kaappi app.scm eval "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 10)"
kaappi app.scm lazy "(define (try a b) (if (= a 0) 1 b)) (try 0 (/ 1 0))"
kaappi app.scm repl                                         # interactive REPL
```

---

## Ecosystem Examples

These examples use Kaappi's ecosystem libraries and external services.
Install dependencies with [thottam](https://github.com/kaappi/kaappi)
before running (see [Setup](#setup) below).

### HTTP File Server

**Difficulty:** Beginner | **Requires:** `kaappi-http`

Serves static files from a directory with MIME type detection and
path traversal protection.

```bash
cd http-file-server
kaappi app.scm 8080 .     # serve current directory on port 8080
# Then: open http://localhost:8080/app.scm
```

### PostgreSQL CRUD

**Difficulty:** Intermediate | **Requires:** `kaappi-pg`, PostgreSQL

Interactive contact book with full CRUD, parameterized queries,
transactions, cursors, and aggregate statistics.

```bash
cd pg-crud
createdb kaappi_demo
kaappi app.scm seed                              # insert sample data
kaappi app.scm list                              # list all contacts
kaappi app.scm search alice                      # search by name/email
kaappi app.scm add "Eve" "eve@test.com" "555-0"  # add contact
kaappi app.scm stats                             # show statistics
```

### Redis Task Queue

**Difficulty:** Intermediate | **Requires:** `kaappi-redis`, Redis

Producer/consumer job queue using Redis lists. Demonstrates the
classic work-queue pattern with status reporting.

```bash
cd redis-task-queue
kaappi app.scm producer   # enqueue 10 tasks
kaappi app.scm worker     # process all tasks
kaappi app.scm status     # show results
```

### REST API

**Difficulty:** Advanced | **Requires:** `kaappi-web`, `kaappi-json`, `kaappi-pg`, `kaappi-redis`, PostgreSQL, Redis

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

---

## Setup

### Pure Scheme examples (no setup needed)

Install Kaappi and run any example directly:

```bash
# Option 1: Download a release binary
# https://github.com/kaappi/kaappi/releases

# Option 2: Build from source
git clone https://github.com/kaappi/kaappi.git
cd kaappi && zig build
# Binary is at zig-out/bin/kaappi
```

### Ecosystem examples

Install the ecosystem libraries with thottam (Kaappi's package manager):

```bash
thottam install kaappi-web     # installs kaappi-http, kaappi-json, kaappi-net
thottam install kaappi-redis
thottam install kaappi-pg

# Verify
thottam list
```

> If thottam is not on your PATH, run it directly: `../kaappi/scripts/thottam install ...`

Ecosystem examples also require their backing services (PostgreSQL,
Redis) to be running. See each example's section above for specifics.

## Adding Your Own

Each example is a self-contained directory with a single `app.scm` entry point:

1. Create a directory: `mkdir my-example`
2. Write `my-example/app.scm` following the conventions (header comment, sections, `user-args` helper, subcommand dispatch)
3. Add a section to this README under the appropriate group
4. Add a smoke test step to `.github/workflows/ci.yml`

See [CLAUDE.md](CLAUDE.md) for full coding conventions.

## Links

- [Kaappi](https://github.com/kaappi/kaappi) — the Scheme interpreter
- [Documentation](https://kaappi-lang.org) — language guide, procedure reference, ecosystem docs
- [Playground](https://kaappi-lang.org/playground/) — try Kaappi in your browser
- [VS Code Extension](https://github.com/kaappi/vscode-kaappi) — syntax highlighting and LSP

## License

[MIT](LICENSE)
