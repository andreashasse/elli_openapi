# elli_openapi
Library for building type-safe HTTP APIs with automatic OpenAPI documentation generation using Elli and Spectra.
This library is not ready for production use, but it wont take long to finish it.

## Usage

1. **Add to your rebar.config dependencies:**

```erlang
{deps, [
    {elli_openapi, "~> 0.1.1"}
]}.
```

2. **Start Elli with elli_openapi_handler as the callback and your routes as arguments to elli_openapi_handler:**

```erlang
%% Define your routes
Routes = [
    {<<"POST">>, <<"/api/users">>, fun user_handler:create_user/4},
    {<<"GET">>, <<"/api/users/{userId}">>, fun user_handler:get_user/4}
],

%% Configure and start Elli, preferably in your supervisor spec.
ElliOpts = [
    {callback, elli_openapi_handler},
    {callback_args, Routes},
    {port, 3000}
],

{ok, Pid} = elli:start_link(ElliOpts).
```

You can optionally pass custom OpenAPI metadata by wrapping `callback_args` in a `{MetaData, Routes}` tuple:

```erlang
MetaData = #{title => <<"My API">>, version => <<"1.0.0">>},
ElliOpts = [
    {callback, elli_openapi_handler},
    {callback_args, {MetaData, Routes}},
    {port, 3000}
].
```

See the `example/` directory for a runnable example application with handler implementations.

## Handler Functions

All handler functions must follow this signature:

```erlang
handler_name(PathArgs, QueryArgs, Headers, Body) -> {StatusCode, ResponseHeaders, ResponseBody}
```

### Arguments

1. **PathArgs** (`map()`): URL path parameters extracted from the route
   - Example: For route `<<"/api/users/{userId}">>`, PathArgs would be `#{userId => <<"42">>}`
   - Empty map `#{}` if no path parameters

2. **QueryArgs** (`map()`): URL query parameters
   - Example: `#{page => 1, per_page => 20}`
   - Declare expected query params in the function spec; undeclared params are ignored

3. **Headers** (`map()`): HTTP request headers with atom keys
   - Example: `#{'Authorization' => <<"Bearer ...">>, 'Content-Type' => <<"application/json">>}`
   - Required headers must be declared in the function spec

4. **Body** (`any()`): Request body, automatically decoded based on the type in your function spec
   - JSON requests: `map()` or record type
   - Plain text requests: `binary()`
   - Bodyless methods (GET, HEAD, etc.): declare as `binary()` — an empty body decodes cleanly to `<<"">>`
   - The library validates and decodes the body according to your spec

### Return Value

Must be a 3-tuple: `{StatusCode, ResponseHeaders, ResponseBody}`

- **StatusCode**: HTTP status code integer (200, 201, 400, etc.)
- **ResponseHeaders**: Map with atom keys (e.g., `#{'Location' => <<"...">>, 'ETag' => <<"...">>}`)
- **ResponseBody**: Response body (record, map, or binary) - will be encoded based on content type

To return different status codes from the same handler, use union types in your function spec where each branch represents a possible response:

```erlang
-spec my_handler(PathArgs, QueryArgs, Headers, Body) ->
    {200, Headers1, SuccessBody}
    | {400, Headers2, ErrorBody}
    | {404, Headers3, NotFoundBody}.
```

### Spec placement

`-spectra()` metadata attributes and `-spec` declarations must appear **before any function clause** in the file. The Erlang compiler processes attributes in declaration order — placing them after a function clause will cause them to be ignored or crash at startup.

```erlang
%% Correct order
-spectra(#{summary => <<"Create user">>}).
-spec create_user(#{}, #{}, #{}, #user{}) -> {201, #{}, #user{}}.
create_user(#{}, #{}, #{}, User) -> ...

%% Wrong — attributes after a function clause are not processed
some_other_function() -> ...
-spectra(#{summary => <<"Create user">>}).   %% too late
-spec create_user(...) -> ...
create_user(...) -> ...
```

For complete handler examples, see `example/src/elli_openapi_demo.erl`.

## Example Application

The `example/` directory contains a runnable demo application showcasing multiple handler implementations including user management, echo, status updates, and item updates with conflict detection.

To run the example:

```bash
make demo
```

The demo starts on port 3000. Access the API documentation at:
- Swagger UI: http://localhost:3000/swagger
- ReDoc: http://localhost:3000/redoc
- OpenAPI JSON: http://localhost:3000/api-docs
