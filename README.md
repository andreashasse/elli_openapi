# elli_openapi
Library for building type-safe HTTP APIs with automatic OpenAPI documentation generation using Elli and Spectra.
This library is not ready for production use, but it wont take long to finish it.

## Usage

1. **Add to your rebar.config dependencies:**

```erlang
{deps, [
    {elli_openapi, {git, "https://github.com/andreashasse/elli_openapi.git", {branch, "main"}}}
]}.
```

2. **Start Elli with elli_openapi_handler as the callback and your routes as arguments to elli_openapi_handler:**

```erlang
%% Define your routes
Routes = [
    {<<"POST">>, <<"/api/users">>, fun my_handler:create_user/3},
    {<<"GET">>, <<"/api/users/{userId}">>, fun my_handler:get_user/3}
],

%% Configure and start Elli, preferably in you supervisor spec.
ElliOpts = [
    {callback, elli_openapi_handler},
    {callback_args, Routes},
    {port, 3000}
],

{ok, Pid} = elli:start_link(ElliOpts).
```

See `src/elli_openapi_demo.erl` for example handler implementations.

## Handler Functions

All handler functions must follow this signature:

```erlang
handler_name(PathArgs, Headers, Body) -> {StatusCode, ResponseHeaders, ResponseBody}
```

### Arguments

1. **PathArgs** (`map()`): URL path parameters extracted from the route
   - Example: For route `<<"/api/users/{userId}">>`, PathArgs would be `#{userId => <<"123">>}`
   - Empty map `#{}` if no path parameters

2. **Headers** (`map()`): HTTP request headers with atom keys
   - Example: `#{'Authorization' => <<"Bearer token">>, 'Content-Type' => <<"application/json">>}`
   - Required headers must be declared in the function spec

3. **Body** (`any()`): Request body, automatically decoded based on the type in your function spec
   - Plain text requests: `binary()`
   - JSON requests: `map()` or record type
   - The library validates and decodes the body according to your spec

### Return Value

Must be a 3-tuple: `{StatusCode, ResponseHeaders, ResponseBody}`

- **StatusCode**: HTTP status code integer (200, 201, 400, etc.)
- **ResponseHeaders**: Map with atom keys (e.g., `#{'Location' => <<"...">>, 'ETag' => <<"...">>}`)
- **ResponseBody**: Response body (record, map, or binary) - will be encoded based on content type

To return different status codes from the same handler, use union types in your function spec where each branch represents a possible response:

```erlang
-spec my_handler(PathArgs, Headers, Body) ->
    {200, Headers1, SuccessBody}
    | {400, Headers2, ErrorBody}
    | {404, Headers3, NotFoundBody}.
```

For complete handler examples, see `src/elli_openapi_demo.erl`.

## Demo

To try out the demo application:

```bash
rebar3 compile
rebar3 shell
```
