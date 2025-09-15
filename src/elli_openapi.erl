-module(elli_openapi).

-export([pelle/0, to_endpoint/1, test_pelle2/0, fun2ms_demo/0]).

-include_lib("erldantic/include/erldantic_internal.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-compile(nowarn_unused_type).

-type my_map() :: #{name := string(), age := integer()}.

-spec pelle() -> term().
pelle() ->
    Endpoint0 = erldantic_openapi:endpoint(get, "/pelle"),
    Endpoint1 = erldantic_openapi:with_request_body(Endpoint0, ?MODULE, {type, my_map, 0}),
    Endpoints = [Endpoint1],
    erldantic_openapi:endpoints_to_openapi(Endpoints).

test_pelle2() ->
    Routes =
        [{post, "/pelle", fun elli_openapi_demo:endpoint/3},
         {get, "/user/{userId}/post/{postId}", fun elli_openapi_demo:endpoint2/3}],
    RouteEndpoints = lists:map(fun (Route) -> {Route, to_endpoint(Route)} end, Routes),
    endpoints_to_file(RouteEndpoints),
    Ms = elli_openapi_matchspec:routes_to_matchspecs(RouteEndpoints),
    Mref = ets:match_spec_compile(Ms),
    MyMap = path_map(RouteEndpoints),
    %% Ah, this 2 will be a string after parsing the path. Should i introduce some flag to all int parsing. Should probably work the same for ranges.
    case ets:match_spec_run([{"user", "Andreas", "post", 2}], Mref) of
        [{Method, Path, PathArgsList}] ->
            PathArgs = maps:from_list(PathArgsList),
            {Fun, Endpoint} = maps:get({Method, Path}, MyMap),
            io:format("Matched, got: ~p ~p ~p~n", [Fun, PathArgs, Endpoint]),
            Fun(PathArgs, #{'User-Agent' => "MyUserAgent"}, {user, "f", "l", 2, []});
        Other ->
            io:format("Did not match, got: ~p~n", [Other]),
            error
    end.

path_map(RouteEndpoints) ->
   lists:foldl(
       fun({{Method, Path, Fun}, Endpoint}, Acc) ->
           maps:put({Method, Path}, {Fun, Endpoint}, Acc)
       end,
       maps:new(),
       RouteEndpoints).


to_endpoint({HttpMethod, Path, CallFun} ) ->
    {Module, Function, Arity} = erlang:fun_info_mfa(CallFun),
    TypeInfo = erldantic_abstract_code:types_in_module(Module),
    {ok, FunctionSpecs} = erldantic_type_info:get_function(TypeInfo, Function, Arity),
    {PathArgs, HeaderArgs, RequestBody, ReturnCode, ReturnBody} = join_function_specs(FunctionSpecs),
    Endpoint0 = erldantic_openapi:endpoint(HttpMethod, Path),
    PathFun = fun(Key, Val, EndpointAcc) ->
             PathArg =
                 #{name => Key,
                   in => path,
                   required => true,
                   schema => Val},

             erldantic_openapi:with_parameter(EndpointAcc, Module, PathArg)
          end,
    EndpointWithPath = maps:fold(PathFun, Endpoint0, to_map(PathArgs)),
    HeaderFun = fun(Key, Val, EndpointAcc) ->
             HeaderArg =
                 #{name => Key,
                   in => header,
                   required => true,
                   schema => Val},

             erldantic_openapi:with_parameter(EndpointAcc, Module, HeaderArg)
          end,
    EndpointWithHeaders = maps:fold(HeaderFun, EndpointWithPath, to_map(HeaderArgs)),
    Endpoint1 = erldantic_openapi:with_request_body(EndpointWithHeaders, Module, RequestBody),
    Endpoint2 =
        erldantic_openapi:with_response(Endpoint1, ReturnCode, "", Module, ReturnBody),
  Endpoint2.

to_map(#ed_map{fields = Fields}) ->
    lists:foldl(fun({map_field_exact, Name, Type}, Acc) -> Acc#{atom_to_list(Name) => Type} end,
                #{},
                Fields).

endpoints_to_file(RouteEndpoints) ->
    Endpoints = lists:map(fun({_Route, Endpoint}) -> Endpoint end, RouteEndpoints),
    {ok, EndpointsJson} = erldantic_openapi:endpoints_to_openapi(Endpoints),
    Json = json:encode(EndpointsJson),
    file:write_file("priv/openapi.json", Json).

fun2ms_demo() ->
    Ms = ets:fun2ms(fun({"user", UserId, "post", PostId}) ->
                       {get,
                        "/user/{UserId}/post/{PostId}",
                        [{'UserId', UserId}, {'PostId', PostId}]}
                    end),
    io:format("This is how the Match spec looks~n~p~n", [Ms]),
    Mref = ets:match_spec_compile(Ms),
    ets:match_spec_run([{"user", "Andreas", "post", 2}], Mref).

join_function_specs([#ed_function_spec{args = [PathArgs, HeaderArgs, Body],
                                       return =
                                           #ed_tuple{fields =
                                                         [#ed_literal{value = ReturnCode},
                                                          _ReturnHeaders,
                                                          ReturnBody]}}]) ->
    {PathArgs, HeaderArgs, Body, ReturnCode, ReturnBody}.
