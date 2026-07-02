# kaappi-examples

Example applications for Kaappi Scheme. Each example is a self-contained
directory with a single `app.scm` entry point.

## Layout

```
<example-name>/
  app.scm          # entry point — always this filename
```

No per-example build files, package manifests, or extra source files.
Each directory name is lowercase, hyphen-separated, and describes the
example's purpose (not the libraries used).

## Running Examples

```bash
# Pure Scheme examples (no dependencies)
kaappi symbolic-differentiation/app.scm demo

# Ecosystem library examples (install deps first)
thottam install kaappi-web kaappi-redis kaappi-pg
kaappi rest-api/app.scm

# If thottam is not on PATH:
../kaappi/zig-out/bin/thottam install ...

# If kaappi is not on PATH:
../kaappi/zig-out/bin/kaappi <example>/app.scm
```

## Code Style

All examples follow a consistent structure:

1. **Header** — `;;;` doc-comment with name, description, usage, prerequisites
2. **Imports** — `(import (scheme base) (scheme write) (scheme process-context) ...)`
3. **Sections** — separated with `;; --- Section Name ---`
4. **Main dispatch** — `user-args` helper strips the script path from
   `(command-line)`, then `(let ((args (user-args))) (cond ...))` matches
   `(car args)` against subcommands with `equal?`
5. **Usage fallback** — `else` clause prints help text

Conventions:
- 2-space indentation (R7RS style)
- `display`/`newline` for output (no structured logging)
- `equal?` for string comparison in CLI dispatch
- Short imperative commit messages

## Adding a New Example

1. Create `<name>/app.scm` following the style above
2. Add a section to `README.md` under the appropriate group
   (`## Pure Scheme Examples` or `## Ecosystem Examples`)
3. Add a smoke test step to `.github/workflows/ci.yml`
4. Pure Scheme examples need no library setup; ecosystem examples
   should document prerequisites in the header comment

## CI

`.github/workflows/ci.yml` runs on push/PR to `main` (macOS runner):
- Builds kaappi from source + all ecosystem libraries
- Smoke tests library imports
- Smoke tests pure Scheme examples

## Kaappi Reference Sources

When writing examples, verify Kaappi features against these sources (not from memory).
All paths are relative to this repo root (`kaappi-examples/`):

- **Kaappi source code**: `../kaappi/` — the Zig implementation
  - `../kaappi/src/` — core runtime, compiler, VM, GC, primitives (~48k lines)
  - `../kaappi/lib/` — portable Scheme SRFI libraries (.sld files)
  - `../kaappi/CLAUDE.md` — detailed build options, architecture, coding patterns
- **Ecosystem libraries**: `../kaappi-*/` — one repo per library
  - `../kaappi-json/lib/` — JSON parser/writer
  - `../kaappi-http/lib/` — HTTP client/server
  - `../kaappi-web/lib/` — web framework
  - `../kaappi-pg/lib/` — PostgreSQL client
  - `../kaappi-redis/lib/` — Redis client
  - Each has `lib/kaappi/<name>.sld` with exported procedure definitions
- **Wiki**: `../wiki/` — Scheme language reference
- **Docs site**: `../kaappi.github.io/` — end-user documentation

## Dependencies

Examples may use:
- **Standard R7RS libraries**: `(scheme base)`, `(scheme write)`,
  `(scheme read)`, `(scheme cxr)`, `(scheme inexact)`,
  `(scheme process-context)`, etc.
- **Kaappi ecosystem libraries**: `(kaappi json)`, `(kaappi http)`,
  `(kaappi pg)`, `(kaappi redis)`, `(kaappi web)`, etc.

Pure Scheme examples have no external dependencies and should be
clearly marked in the README.
