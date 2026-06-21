# kaappi-examples

Example applications for [Kaappi Scheme](https://github.com/kaappi/kaappi),
demonstrating the [Redis](https://github.com/kaappi/kaappi-redis),
[PostgreSQL](https://github.com/kaappi/kaappi-pg), and
[HTTP](https://github.com/kaappi/kaappi-http) libraries.

## Setup

Clone and build all libraries:

```bash
cd /path/to/kaappi
(cd kaappi && zig build)
(cd kaappi-redis && make)
(cd kaappi-pg && make)
(cd kaappi-http && make)
```

Set the library and dynamic linker paths:

```bash
export DYLD_LIBRARY_PATH=../kaappi-redis:../kaappi-pg:../kaappi-http:$(pg_config --libdir)

alias krun='../kaappi/zig-out/bin/kaappi \
  --lib-path ../kaappi-redis/lib \
  --lib-path ../kaappi-pg/lib \
  --lib-path ../kaappi-http/lib'
```

## Examples

### REST API (Redis + PostgreSQL + HTTP)

A full REST API server with PostgreSQL storage and Redis caching.

```bash
createdb kaappi_demo
redis-server --daemonize yes
cd rest-api && krun app.scm

# In another terminal:
curl -X POST -d "name=Alice&email=alice@example.com" http://localhost:8080/users
curl http://localhost:8080/users
curl http://localhost:8080/users/1    # cached in Redis
```

See [rest-api/README.md](rest-api/README.md) for full documentation.

### Redis Task Queue

Producer/consumer job queue using Redis lists.

```bash
cd redis-task-queue
krun app.scm producer   # enqueue 10 tasks
krun app.scm worker     # process all tasks
krun app.scm status     # show results
```

### PostgreSQL CRUD

Interactive contact book with full CRUD, search, and statistics.

```bash
cd pg-crud
createdb kaappi_demo
krun app.scm seed                              # insert sample data
krun app.scm list                              # list all contacts
krun app.scm search alice                      # search by name/email
krun app.scm add "Eve" "eve@test.com" "555-0"  # add contact
krun app.scm stats                             # show statistics
```

### HTTP File Server

Serves static files from a directory with MIME type detection.

```bash
cd http-file-server
krun app.scm 8080 .     # serve current directory on port 8080
# Then: open http://localhost:8080/app.scm
```
