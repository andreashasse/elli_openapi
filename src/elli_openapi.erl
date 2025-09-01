-module(elli_openapi).

-export([pelle/0, pelle2/1, test_pelle2/0]).

-include_lib("erldantic/include/erldantic_internal.hrl").

-compile(nowarn_unused_type).

-type my_map() :: #{name := string(), age := integer()}.

-spec pelle() -> term().
pelle() ->
    Endpoint0 = erldantic_openapi:endpoint(get, "/pelle"),
    Endpoint1 = erldantic_openapi:with_request_body(Endpoint0, ?MODULE, {type, my_map, 0}),
    Endpoints = [Endpoint1],
    erldantic_openapi:endpoints_to_openapi(Endpoints).

test_pelle2() ->
    pelle2(fun elli_openapi_demo:endpoint/1).

pelle2(Fun) ->
    {Module, Function, Arity} = erlang:fun_info_mfa(Fun),
    TypeInfo = erldantic_abstract_code:types_in_module(Module),
    {ok, FunctionSpecs} = erldantic_type_info:get_function(TypeInfo, Function, Arity),
    {RequestBody, ReturnCode, ReturnBody} = join_function_specs(FunctionSpecs),
    Endpoint0 = erldantic_openapi:endpoint(get, "/pelle"),
    Endpoint1 = erldantic_openapi:with_request_body(Endpoint0, Module, RequestBody),
    Endpoint2 =
        erldantic_openapi:with_response(Endpoint1, ReturnCode, "", Module, ReturnBody),
    Endpoints = [Endpoint2],
    erldantic_openapi:endpoints_to_openapi(Endpoints).

join_function_specs([#ed_function_spec{args = [Arg],
                                       return =
                                           #ed_tuple{fields =
                                                         [#ed_literal{value = ReturnCode},
                                                          _ReturnHeaders,
                                                          ReturnBody]}}]) ->
    {Arg, ReturnCode, ReturnBody}.
