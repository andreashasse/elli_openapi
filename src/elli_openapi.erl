-module(elli_openapi).

-export([pelle/0]).
-compile(nowarn_unused_type).
-type my_map() :: #{name := string(), age := integer()}.
-spec pelle() -> term().
pelle() ->
    Endpoint0 = erldantic_openapi:endpoint(get, "/pelle"),
    Endpoint1 = erldantic_openapi:with_request_body(Endpoint0, {?MODULE, my_map}),
    Endpoints = [Endpoint1],
    erldantic_openapi:endpoints_to_openapi(Endpoints).
