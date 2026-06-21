# REST API — User Management

A REST API server backed by PostgreSQL with Redis caching, built entirely
in Kaappi Scheme. Demonstrates all four libraries working together.

## Prerequisites

```bash
# Build the libraries (kaappi-json is pure Scheme, no build needed)
(cd ../kaappi-redis && make)
(cd ../kaappi-pg && make)
(cd ../kaappi-http && make)

# Start services
redis-server --daemonize yes
createdb kaappi_demo
```

## Run

```bash
export DYLD_LIBRARY_PATH=../kaappi-redis:../kaappi-pg:../kaappi-http:$(pg_config --libdir)
kaappi --lib-path ../kaappi-redis/lib \
       --lib-path ../kaappi-pg/lib \
       --lib-path ../kaappi-http/lib \
       --lib-path ../kaappi-json/lib \
       app.scm
```

## Endpoints

```bash
# Health check
curl http://localhost:8080/health
# => {"status":"ok"}

# Create users (JSON body)
curl -X POST -H "Content-Type: application/json" \
     -d '{"name":"Alice","email":"alice@example.com"}' \
     http://localhost:8080/users

# Create users (form body also works)
curl -X POST -d "name=Bob&email=bob@example.com" http://localhost:8080/users

# List all users
curl http://localhost:8080/users
# => [{"id":1,"name":"Alice","email":"alice@example.com","created_at":"..."},...]

# Get user by ID (cached in Redis for 60s)
curl http://localhost:8080/users/1

# Delete user (invalidates cache)
curl -X DELETE http://localhost:8080/users/1
# => {"deleted":true}
```
