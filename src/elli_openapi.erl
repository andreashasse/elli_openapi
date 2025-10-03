-module(elli_openapi).

-export([demo/0, setup_routes/1, route_call/1]).

-include_lib("erldantic/include/erldantic_internal.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("elli/include/elli.hrl").

-compile(nowarn_unused_type).

-record(handler_type, {
    mfa :: mfa(),
    path_args :: #ed_map{},
    header_args :: #ed_map{},
    request_body :: erldantic:ed_type(),
    return_code :: integer(),
    return_body :: erldantic:ed_type()
}).

-type erldantic_openapi__endpoint_spec() :: map().

%% FIXME: Export types from erldantic_openapi.

demo() ->
    Routes =
        [
            {~"POST", ~"/pelle", fun elli_openapi_demo:endpoint/2},
            {~"GET", ~"/user/{userId}/post/{postId}", fun elli_openapi_demo:endpoint2/3}
        ],
    ElliRequest =
        #req{
            args = [],
            method = 'GET',
            pid = self(),
            callback = {elli_example_callback, []},
            raw_path = <<"mojs">>,
            version = {1, 1},
            body =
                ~"{\"access\":[\"read\"],\"first_name\":\"Andreas\",\"last_name\":\"Hasselberg\"}",
            headers = [{~"User-Agent", ~"Firefox"}],
            path = [~"user", ~"Andreas", ~"post", ~"2"]
        },
    setup_routes(Routes),
    route_call(ElliRequest).

setup_routes(Routes) ->
    RouteEndpoints =
        lists:map(
            fun(Route) ->
                HandlerType = to_handler_type(Route),
                {Route, to_endpoint(Route, HandlerType), HandlerType}
            end,
            Routes
        ),
    endpoints_to_file(RouteEndpoints),
    Mref = to_matchspec(RouteEndpoints),
    MyMap = path_map(RouteEndpoints),
    persistent_term:put(?MODULE, {Mref, MyMap}),
    ok.

route_call(ElliRequest) ->
    {Mref, MyMap} = persistent_term:get(?MODULE),
    Method = ensure_binary(elli_request:method(ElliRequest)),
    Path = list_to_tuple(elli_request:path(ElliRequest)),
    case ets:match_spec_run([{Method, Path}], Mref) of
        [{RoutePath, HttpPathArgsList}] ->
            HttpPathArgs = maps:from_list(HttpPathArgsList),
            {Fun, _Endpoint, HandlerType} = maps:get({Method, RoutePath}, MyMap),
            case check_types(HandlerType, HttpPathArgs, ElliRequest) of
                {ok, PathArgs, Headers, Body} ->
                    Fun(PathArgs, Headers, Body);
                {error, ErldanticErrors} ->
                    {400, [], erldantic_error_to_response_body(ErldanticErrors)}
            end;
        [] ->
            {404, [], ~"Not Found"}
    end.

%% FIXME: very debuggish
erldantic_error_to_response_body(Errors) ->
    io_lib:format("Errors: ~p", [Errors]).

ensure_binary(Bin) when is_binary(Bin) ->
    Bin;
ensure_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8).

check_types(HandlerType, PathArgs, ElliRequest) ->
    #handler_type{
        mfa = {Module, _, _},
        path_args = PathArgsType,
        header_args = HeadersType,
        request_body = RequestBodyType
    } =
        HandlerType,

    case decode_path_args(Module, PathArgs, PathArgsType) of
        {ok, DecodePathArgs} ->
            case decode_headers(Module, HeadersType, elli_request:headers(ElliRequest)) of
                {ok, DecodedHeader} ->
                    Body =
                        json:decode(
                            elli_request:body(ElliRequest)
                        ),

                    case erldantic_json:from_json(Module, RequestBodyType, Body) of
                        {ok, DecodedBody} ->
                            {ok, DecodePathArgs, DecodedHeader, DecodedBody};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

decode_path_args(Module, PathArgs, PathArgsType) ->
    erldantic_util:fold_until_error(
        fun({map_field_exact, FieldName, Type}, Acc) ->
            case maps:find(FieldName, PathArgs) of
                {ok, PathArg} ->
                    case
                        erldantic_binary_string:from_binary_string(
                            Module,
                            Type,
                            PathArg
                        )
                    of
                        {ok, DecodedPathArgs} ->
                            {ok, maps:put(FieldName, DecodedPathArgs, Acc)};
                        {error, _} = Error ->
                            Error
                    end;
                error ->
                    {error, {missing_path_arg, FieldName}}
            end
        end,
        #{},
        PathArgsType#ed_map.fields
    ).

decode_headers(Module, HeadersType, Headers) ->
    erldantic_util:fold_until_error(
        fun({map_field_exact, FieldName, Type}, Acc) ->
            HeaderName = atom_to_binary(FieldName),
            case lists:keyfind(HeaderName, 1, Headers) of
                {HeaderName, HeaderValue} ->
                    case
                        erldantic_binary_string:from_binary_string(
                            Module,
                            Type,
                            HeaderValue
                        )
                    of
                        {ok, DecodedHeader} ->
                            {ok, maps:put(FieldName, DecodedHeader, Acc)};
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {error, {missing_header, FieldName}}
            end
        end,
        #{},
        HeadersType#ed_map.fields
    ).

to_matchspec(RouteEndpoints) ->
    Ms = elli_openapi_matchspec:routes_to_matchspecs(RouteEndpoints),
    ets:match_spec_compile(Ms).

path_map(RouteEndpoints) ->
    lists:foldl(
        fun({{Method, Path, Fun}, Endpoint, HandlerType}, Acc) ->
            maps:put({Method, Path}, {Fun, Endpoint, HandlerType}, Acc)
        end,
        maps:new(),
        RouteEndpoints
    ).

-spec to_handler_type({atom(), binary(), fun()}) -> #handler_type{}.
to_handler_type({_HttpMethod, _Path, CallFun}) ->
    {Module, Function, Arity} = MFA = erlang:fun_info_mfa(CallFun),
    TypeInfo = erldantic_abstract_code:types_in_module(Module),
    {ok, FunctionSpecs} = erldantic_type_info:get_function(TypeInfo, Function, Arity),
    join_function_specs(MFA, FunctionSpecs).

-spec to_endpoint({binary(), binary(), fun()}, #handler_type{}) ->
    erldantic_openapi__endpoint_spec().
to_endpoint(
    {HttpMethod, Path, _CallFun},
    #handler_type{
        mfa = {Module, _Function, _Arity},
        path_args = PathArgs,
        header_args = HeaderArgs,
        request_body = RequestBody,
        return_code = ReturnCode,
        return_body = ReturnBody
    }
) ->
    Endpoint0 = erldantic_openapi:endpoint(HttpMethod, Path),
    PathFun =
        fun(Key, Val, EndpointAcc) ->
            PathArg =
                #{
                    name => Key,
                    in => path,
                    required => true,
                    module => Module,
                    schema => Val
                },

            erldantic_openapi:with_parameter(EndpointAcc, Module, PathArg)
        end,
    EndpointWithPath = maps:fold(PathFun, Endpoint0, to_map(PathArgs)),
    HeaderFun =
        fun(Key, Val, EndpointAcc) ->
            HeaderArg =
                #{
                    name => Key,
                    in => header,
                    %% FIXME: handle optional headers
                    required => true,
                    module => Module,
                    schema => Val
                },

            erldantic_openapi:with_parameter(EndpointAcc, Module, HeaderArg)
        end,
    EndpointWithHeaders = maps:fold(HeaderFun, EndpointWithPath, to_map(HeaderArgs)),
    Endpoint1 = erldantic_openapi:with_request_body(EndpointWithHeaders, Module, RequestBody),
    Endpoint2 =
        erldantic_openapi:with_response(Endpoint1, ReturnCode, <<"">>, Module, ReturnBody),
    Endpoint2.

to_map(#ed_map{fields = Fields}) ->
    lists:foldl(
        fun({map_field_exact, Name, Type}, Acc) -> Acc#{atom_to_list(Name) => Type} end,
        #{},
        Fields
    ).

endpoints_to_file(RouteEndpoints) ->
    Endpoints =
        lists:map(fun({_Route, Endpoint, _HandlerType}) -> Endpoint end, RouteEndpoints),
    {ok, EndpointsJson} = erldantic_openapi:endpoints_to_openapi(Endpoints),
    Json = json:encode(EndpointsJson),
    file:write_file("priv/openapi.json", Json).

join_function_specs(
    MFA,
    [
        #ed_function_spec{
            args = [PathArgs, HeaderArgs, Body],
            return =
                #ed_tuple{
                    fields =
                        [
                            #ed_literal{value = ReturnCode},
                            _ReturnHeaders,
                            ReturnBody
                        ]
                }
        }
    ]
) ->
    #handler_type{
        mfa = MFA,
        path_args = PathArgs,
        header_args = HeaderArgs,
        request_body = Body,
        return_code = ReturnCode,
        return_body = ReturnBody
    }.
