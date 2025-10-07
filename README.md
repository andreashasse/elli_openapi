# elli_openapi

**Demo repository for [erldantic_openapi](https://github.com/andreashasse/erldantic)**

This project demonstrates how to use `erldantic_openapi` to automatically generate OpenAPI specifications from Erlang function type specs and build type-safe HTTP APIs with the Elli web server.

## Features

- **Type-safe HTTP handlers** - Request and response validation based on Erlang type specifications
- **Automatic OpenAPI spec generation** - OpenAPI 3.0 documentation generated from function specs at compile time
- **Runtime type validation** - Automatic validation and conversion of path parameters, headers, and request/response bodies
- **Swagger UI integration** - Interactive API documentation available at `/swagger`
- **OpenAPI JSON endpoint** - Machine-readable spec available at `/api-docs`

## Dependencies

- [erldantic](https://github.com/andreashasse/erldantic) - Type introspection and validation for Erlang
- [elli](https://github.com/elli-lib/elli) - Lightweight HTTP server for Erlang

## Build & Run

```bash
rebar3 compile
rebar3 shell
```

The server starts on port 3000 with two demo endpoints:
- `POST /pelle`
- `GET /user/{userId}/post/{postId}`

## Usage

### View Swagger UI

Open your browser to:
```
http://localhost:3000/swagger
```

### Get OpenAPI Spec

```bash
curl http://localhost:3000/api-docs
```

### Test Endpoints

```bash
# POST endpoint
curl -X POST http://localhost:3000/pelle \
  -H "Content-Type: application/json" \
  -d '{"name":"John","shirt_size":"medium"}'

# GET endpoint with path params and headers
curl -X GET http://localhost:3000/user/Andreas/post/2 \
  -H "Content-Type: application/json" \
  -H "User-Agent: Firefox" \
  -d '{"access":["read"],"first_name":"Andreas","last_name":"Hasselberg"}'
```

### Test Validation Errors

The runtime type validation automatically rejects invalid requests with 400 Bad Request:

```bash
# Invalid enum value for shirt_size (should be small|medium|large)
curl -X POST http://localhost:3000/pelle \
  -H "Content-Type: application/json" \
  -d '{"name":"John","shirt_size":"xlarge"}'

# Missing required field (name)
curl -X POST http://localhost:3000/pelle \
  -H "Content-Type: application/json" \
  -d '{"shirt_size":"medium"}'

# Invalid type for path parameter (postId must be integer)
curl -X GET http://localhost:3000/user/Andreas/post/abc \
  -H "Content-Type: application/json" \
  -H "User-Agent: Firefox" \
  -d '{"access":["read"],"first_name":"Andreas","last_name":"Hasselberg"}'

# Invalid enum in access array (should be read|write|delete)
curl -X GET http://localhost:3000/user/Andreas/post/2 \
  -H "Content-Type: application/json" \
  -H "User-Agent: Firefox" \
  -d '{"access":["admin"],"first_name":"Andreas","last_name":"Hasselberg"}'

# Missing required header (User-Agent)
curl -X GET http://localhost:3000/user/Andreas/post/2 \
  -H "Content-Type: application/json" \
  -d '{"access":["read"],"first_name":"Andreas","last_name":"Hasselberg"}'
```

## How It Works

1. Define your handler functions with complete type specs in Erlang
2. Register routes with `elli_openapi:setup_routes/1`
3. erldantic_openapi introspects the type specs and generates OpenAPI documentation
4. Runtime validation ensures requests and responses conform to the specs
5. The generated OpenAPI spec is served at `/api-docs` and visualized via Swagger UI

See `src/elli_openapi_demo.erl` for example handler implementations.
