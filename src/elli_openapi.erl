-module(elli_openapi).

-export([demo/0, setup_routes/1, route_call/1]).

-include_lib("erldantic/include/erldantic_internal.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("elli/include/elli.hrl").

-compile(nowarn_unused_type).

-record(handler_type,
        {mfa :: mfa(),
         path_args :: #ed_map{},
         header_args :: #ed_map{},
         request_body :: erldantic:ed_type(),
         return_code :: integer(),
         return_body :: erldantic:ed_type()}).

demo() ->
    Routes =
        [{post, "/pelle", fun elli_openapi_demo:endpoint/3},
         {get, "/user/{userId}/post/{postId}", fun elli_openapi_demo:endpoint2/3}],
    ElliRequest =
        #req{args = [],
             method = 'GET',
             pid = self(),
             callback = {elli_example_callback, []},
             raw_path = <<"mojs">>,
             version = {1, 1},
             body = <<"{\"access\":[\"read\"],\"first_name\":\"Andreas\",\"last_name\":\"Hasselberg\",\"age\":42}">>,
             headers = [],
             path = [<<"mojs">>]},
    setup_routes(Routes),
    route_call(ElliRequest).

setup_routes(Routes) ->
    RouteEndpoints =
        lists:map(fun(Route) ->
                     HandlerType = to_handler_type(Route),
                     {Route, to_endpoint(Route, HandlerType), HandlerType}
                  end,
                  Routes),
    endpoints_to_file(RouteEndpoints),
    Mref = to_matchspec(RouteEndpoints),
    MyMap = path_map(RouteEndpoints),
    persistent_term:put(?MODULE, {Mref, MyMap}),
    ok.

route_call(ElliRequest) ->
    {Mref, MyMap} = persistent_term:get(?MODULE),
    Method = get,
    case ets:match_spec_run([{Method, {"user", "Andreas", "post", "2"}}], Mref) of
        [{Path, HttpPathArgsList}] ->
            HttpPathArgs = maps:from_list(HttpPathArgsList),
            {Fun, Endpoint, HandlerType} = maps:get({Method, Path}, MyMap),
            io:format("Matched~nFun:  ~p~nPath: ~p~nEnd:  ~p~nType: ~p ~n",
                      [Fun, HttpPathArgs, Endpoint, HandlerType]),
            case check_types(HandlerType, HttpPathArgs, ElliRequest) of
                {ok, PathArgs, _Headers, Body} ->
                    Fun(PathArgs, #{'User-Agent' => "MyUserAgent"}, Body);
                {error, ErrorCode, ErrorBody} ->
                    {ErrorCode, [], ErrorBody}
            end;
        Other ->
            io:format("Did not match, got: ~p~n", [Other]),
            error
    end.

check_types(HandlerType, PathArgs, ElliRequest) ->
    #handler_type{mfa = {Module, _, _},
                  path_args = PathArgsType,
                  header_args = _HeaderArgsType,
                  request_body = RequestBodyType} = HandlerType,
    code:ensure_loaded(Module),
    Body = json:decode(elli_request:body(ElliRequest)),
    case erldantic_json:from_json(Module, RequestBodyType, Body) of
        {ok, DecodedBody} ->
            case decode_path_args(Module, PathArgs, PathArgsType) of
                {ok, DecodePathArgs} ->
                    {ok, DecodePathArgs, #{}, DecodedBody};
                {error, _} = Error ->
                    Error
            end;
       {error, _} = Error ->
            Error
    end.

decode_path_args(Module, PathArgs, PathArgsType) ->
    erldantic_util:fold_until_error(
        fun({map_field_exact,FieldName,Type}, Acc) ->
            case maps:find(FieldName, PathArgs) of
                {ok, PathArg} ->
                    case erldantic_string:from_string(Module, Type, PathArg) of
                        {ok, DecodedPathArgs} ->
                            {ok, maps:put(FieldName, DecodedPathArgs, Acc)};
                        {error, _} = Error ->
                            Error
                    end;
                error ->
                    {error, missing_path_arg, FieldName}
            end
        end,
        #{}, PathArgsType#ed_map.fields).


to_matchspec(RouteEndpoints) ->
    Ms = elli_openapi_matchspec:routes_to_matchspecs(RouteEndpoints),
    ets:match_spec_compile(Ms).

path_map(RouteEndpoints) ->
    lists:foldl(fun({{Method, Path, Fun}, Endpoint, HandlerType}, Acc) ->
                   maps:put({Method, Path}, {Fun, Endpoint, HandlerType}, Acc)
                end,
                maps:new(),
                RouteEndpoints).

to_handler_type({_HttpMethod, _Path, CallFun}) ->
    {Module, Function, Arity} = MFA = erlang:fun_info_mfa(CallFun),
    TypeInfo = erldantic_abstract_code:types_in_module(Module),
    {ok, FunctionSpecs} = erldantic_type_info:get_function(TypeInfo, Function, Arity),
    join_function_specs(MFA, FunctionSpecs).

to_endpoint({HttpMethod, Path, _CallFun},
            #handler_type{mfa = {Module, _Function, _Arity},
                          path_args = PathArgs,
                          header_args = HeaderArgs,
                          request_body = RequestBody,
                          return_code = ReturnCode,
                          return_body = ReturnBody}) ->
    Endpoint0 = erldantic_openapi:endpoint(HttpMethod, Path),
    PathFun =
        fun(Key, Val, EndpointAcc) ->
           PathArg =
               #{name => Key,
                 in => path,
                 required => true,
                 schema => Val},

           erldantic_openapi:with_parameter(EndpointAcc, Module, PathArg)
        end,
    EndpointWithPath = maps:fold(PathFun, Endpoint0, to_map(PathArgs)),
    HeaderFun =
        fun(Key, Val, EndpointAcc) ->
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
    lists:foldl(fun({map_field_exact, Name, Type}, Acc) -> Acc#{atom_to_list(Name) => Type}
                end,
                #{},
                Fields).

endpoints_to_file(RouteEndpoints) ->
    Endpoints =
        lists:map(fun({_Route, Endpoint, _HandlerType}) -> Endpoint end, RouteEndpoints),
    {ok, EndpointsJson} = erldantic_openapi:endpoints_to_openapi(Endpoints),
    Json = json:encode(EndpointsJson),
    file:write_file("priv/openapi.json", Json).

join_function_specs(MFA,
                    [#ed_function_spec{args = [PathArgs, HeaderArgs, Body],
                                       return =
                                           #ed_tuple{fields =
                                                         [#ed_literal{value = ReturnCode},
                                                          _ReturnHeaders,
                                                          ReturnBody]}}]) ->
    #handler_type{mfa = MFA,
                  path_args = PathArgs,
                  header_args = HeaderArgs,
                  request_body = Body,
                  return_code = ReturnCode,
                  return_body = ReturnBody}.
