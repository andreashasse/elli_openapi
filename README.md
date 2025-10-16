# elli_openapi
Library for building type-safe HTTP APIs with automatic OpenAPI documentation generation using Elli and Erldantic.
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

## Demo

To try out the demo application:

```bash
rebar3 compile
rebar3 shell
```

## TODO
- [ ] Validate response codes
- [ ] Clean up the code
- [ ] How should extra headers (not in spec) be handled?
