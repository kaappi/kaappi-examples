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
../kaappi/scripts/thottam install ...

# If kaappi is not on PATH:
../kaappi/zig-out/bin/kaappi <example>/app.scm
```

## Code Style

All examples follow a consistent structure:

1. **Header** — `;;;` doc-comment with name, description, usage, prerequisites
2. **Imports** — `(import (scheme base) (scheme write) (scheme process-context) ...)`
3. **Sections** — separated with `;; --- Section Name ---`
4. **Main dispatch** — `(let ((args (command-line))) (cond ...))` matching
   `(list-ref args 2)` against subcommands with `equal?`
5. **Usage fallback** — `else` clause prints help text

Conventions:
- 2-space indentation (R7RS style)
- `display`/`newline` for output (no structured logging)
- `equal?` for string comparison in CLI dispatch
- Short imperative commit messages

## Adding a New Example

1. Create `<name>/app.scm` following the style above
2. Add a section to `README.md` under `## Examples`
3. Add a smoke test step to `.github/workflows/ci.yml`
4. Pure Scheme examples need no library setup; ecosystem examples
   should document prerequisites in the header comment

## CI

`.github/workflows/ci.yml` runs on push/PR to `main` (macOS runner):
- Builds kaappi from source + all ecosystem libraries
- Smoke tests library imports
- Smoke tests pure Scheme examples

## Dependencies

Examples may use:
- **Standard R7RS libraries**: `(scheme base)`, `(scheme write)`,
  `(scheme read)`, `(scheme cxr)`, `(scheme inexact)`,
  `(scheme process-context)`, etc.
- **Kaappi ecosystem libraries**: `(kaappi json)`, `(kaappi http)`,
  `(kaappi pg)`, `(kaappi redis)`, `(kaappi web)`, etc.

Pure Scheme examples have no external dependencies and should be
clearly marked in the README.
