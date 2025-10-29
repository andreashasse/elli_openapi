-module(elli_openapi).

-export([
    setup_routes/1,
    route_call/1,
    to_handler_type/1,
    to_endpoint/2,
    generate_openapi_spec/2
]).

-ignore_xref([to_handler_type/1, to_endpoint/2, generate_openapi_spec/2]).

-include_lib("spectra/include/spectra_internal.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("elli/include/elli.hrl").

-compile(nowarn_unused_type).

-type content_type() :: plain | json.

-record(response_spec, {
    status_code :: integer(),
    headers :: spectra:sp_type(),
    body :: spectra:sp_type(),
    content_type :: content_type()
}).

-record(handler_type, {
    mfa :: mfa(),
    path_args :: #sp_map{},
    header_args :: #sp_map{},
    request_body :: spectra:sp_type(),
    request_content_type :: content_type(),
    responses :: #{integer() => #response_spec{}}
}).

-type spectra_openapi__endpoint_spec() :: map().

setup_routes(Routes) ->
    RouteEndpoints =
        lists:map(
            fun(Route) ->
                HandlerType = to_handler_type(Route),
                {Route, to_endpoint(Route, HandlerType), HandlerType}
            end,
            Routes
        ),
    MetaData = #{title => ~"My API", version => ~"1.0.0"},
    endpoints_to_file(MetaData, Routes),
    Mref = to_matchspec(RouteEndpoints),
    MyMap = path_map(RouteEndpoints),
    persistent_term:put(?MODULE, {Mref, MyMap}),
    ok.

route_call(ElliRequest) ->
    {Mref, MyMap} = persistent_term:get(?MODULE),
    Method = ensure_binary(elli_request:method(ElliRequest)),
    Path = list_to_tuple(elli_request:path(ElliRequest)),
    case ets:match_spec_run([{to_spectra_http_method(Method), Path}], Mref) of
        [{RoutePath, HttpPathArgsList}] ->
            HttpPathArgs = maps:from_list(HttpPathArgsList),
            {Fun, _Endpoint, HandlerType} = maps:get({Method, RoutePath}, MyMap),
            case check_types(HandlerType, HttpPathArgs, ElliRequest) of
                {ok, PathArgs, Headers, Body} ->
                    Response = Fun(PathArgs, Headers, Body),
                    check_and_convert_response(HandlerType, Response);
                {error, ErldanticErrors} ->
                    {400, [], spectra_error_to_response_body(ErldanticErrors)}
            end;
        [] ->
            {404, [], ~"Not Found"}
    end.

check_and_convert_response(HandlerType, {HttpCode, Headers, Body}) ->
    #handler_type{
        mfa = {Module, _, _},
        responses = Responses
    } =
        HandlerType,
    case maps:find(HttpCode, Responses) of
        {ok, ResponseSpec} ->
            #response_spec{
                headers = ReturnHeadersType,
                body = ReturnBodyType,
                content_type = ResponseContentType
            } = ResponseSpec,
            EncodeFormat =
                case ResponseContentType of
                    plain -> binary_string;
                    json -> json
                end,
            case spectra:encode(EncodeFormat, Module, ReturnBodyType, Body) of
                {ok, EncodedBody} ->
                    case encode_headers(Module, ReturnHeadersType, Headers) of
                        {ok, EncodedHeaders} ->
                            {HttpCode, EncodedHeaders, EncodedBody};
                        {error, ErldanticErrors} ->
                            {500, [], spectra_error_to_response_body(ErldanticErrors)}
                    end;
                {error, ErldanticErrors} ->
                    {500, [], spectra_error_to_response_body(ErldanticErrors)}
            end;
        error ->
            ErrorMsg = iolist_to_binary(
                io_lib:format("Invalid status code ~p returned by handler", [HttpCode])
            ),
            {500, [], ErrorMsg}
    end.

encode_headers(Module, ReturnHeadersType, Headers) ->
    spectra_util:fold_until_error(
        fun({FieldType, FieldName, Type}, Acc) when
            FieldType =:= map_field_exact orelse FieldType =:= map_field_assoc
        ->
            case maps:find(FieldName, Headers) of
                {ok, HeaderValue} ->
                    case spectra:encode(binary_string, Module, Type, HeaderValue) of
                        {ok, EncodedHeader} ->
                            HeaderName = atom_to_binary(FieldName),
                            {ok, [{HeaderName, EncodedHeader} | Acc]};
                        {error, _} = Error ->
                            Error
                    end;
                error when FieldType =:= map_field_exact ->
                    {error, {missing_header, FieldName}};
                error ->
                    {ok, Acc}
            end
        end,
        [],
        ReturnHeadersType#sp_map.fields
    ).

spectra_error_to_response_body(Errors) ->
    try
        iolist_to_binary(io_lib:format("Errors: ~p", [Errors]))
    catch
        _:_ ->
            <<"Error formatting error message">>
    end.

ensure_binary(Bin) when is_binary(Bin) ->
    Bin;
ensure_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8).

check_types(HandlerType, PathArgs, ElliRequest) ->
    #handler_type{
        mfa = {Module, _, _},
        path_args = PathArgsType,
        header_args = HeadersType,
        request_body = RequestBodyType,
        request_content_type = RequestContentType
    } =
        HandlerType,

    case decode_path_args(Module, PathArgs, PathArgsType) of
        {ok, DecodePathArgs} ->
            case decode_headers(Module, HeadersType, elli_request:headers(ElliRequest)) of
                {ok, DecodedHeader} ->
                    case decode_body(Module, RequestBodyType, RequestContentType, ElliRequest) of
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

decode_body(Module, RequestBodyType, ExpectedContentType, ElliRequest) ->
    Body = elli_request:body(ElliRequest),
    ActualContentType = get_content_type(ElliRequest),

    case {ExpectedContentType, ActualContentType} of
        {json, {ok, <<"application/json">>}} ->
            spectra:decode(json, Module, RequestBodyType, Body);
        {json, {error, missing}} ->
            spectra:decode(json, Module, RequestBodyType, Body);
        {plain, {ok, <<"text/", _/binary>>}} ->
            spectra:decode(binary_string, Module, RequestBodyType, Body);
        {plain, {error, missing}} ->
            spectra:decode(binary_string, Module, RequestBodyType, Body);
        {ExpectedType, {ok, ActualType}} ->
            ExpectedMime = content_type_to_mime(ExpectedType),
            {error, [
                {sp_error, [], decode_error, #{
                    reason => content_type_mismatch,
                    expected => ExpectedMime,
                    got => ActualType
                }}
            ]};
        {_, {error, missing}} ->
            {error, [
                {sp_error, [], decode_error, #{
                    reason => missing_content_type,
                    expected => content_type_to_mime(ExpectedContentType)
                }}
            ]}
    end.

get_content_type(ElliRequest) ->
    Headers = elli_request:headers(ElliRequest),
    case lists:keyfind(<<"Content-Type">>, 1, Headers) of
        {_, ContentTypeHeader} ->
            [MediaType | _] = binary:split(ContentTypeHeader, <<";">>),
            ContentType = string:trim(MediaType, both, " \t"),
            {ok, ContentType};
        false ->
            {error, missing}
    end.

decode_path_args(Module, PathArgs, PathArgsType) ->
    spectra_util:fold_until_error(
        fun({map_field_exact, FieldName, Type}, Acc) ->
            case maps:find(FieldName, PathArgs) of
                {ok, PathArg} ->
                    case spectra:decode(binary_string, Module, Type, PathArg) of
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
        PathArgsType#sp_map.fields
    ).

decode_headers(Module, HeadersType, Headers) ->
    spectra_util:fold_until_error(
        fun({map_field_exact, FieldName, Type}, Acc) ->
            HeaderName = atom_to_binary(FieldName),
            case lists:keyfind(HeaderName, 1, Headers) of
                {HeaderName, HeaderValue} ->
                    case spectra:decode(binary_string, Module, Type, HeaderValue) of
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
        HeadersType#sp_map.fields
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
    TypeInfo = spectra_abstract_code:types_in_module(Module),
    {ok, FunctionSpecs} = spectra_type_info:get_function(TypeInfo, Function, Arity),
    join_function_specs(MFA, FunctionSpecs).

-spec to_endpoint({binary(), binary(), fun()}, #handler_type{}) ->
    spectra_openapi__endpoint_spec().
to_endpoint(
    {HttpMethod, Path, _CallFun},
    #handler_type{
        mfa = {Module, _Function, _Arity},
        path_args = PathArgs,
        header_args = HeaderArgs,
        request_body = RequestBody,
        request_content_type = RequestContentType,
        responses = Responses
    }
) ->
    Endpoint0 = spectra_openapi:endpoint(to_spectra_http_method(HttpMethod), Path),
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

            spectra_openapi:with_parameter(EndpointAcc, Module, PathArg)
        end,
    EndpointWithPath = maps:fold(PathFun, Endpoint0, to_map(PathArgs)),
    HeaderFun =
        fun({FieldType, Name, Type}, EndpointAcc) when
            FieldType =:= map_field_exact orelse FieldType =:= map_field_assoc
        ->
            HeaderArg =
                #{
                    name => atom_to_binary(Name),
                    in => header,
                    required => FieldType =:= map_field_exact,
                    module => Module,
                    schema => Type
                },
            spectra_openapi:with_parameter(EndpointAcc, Module, HeaderArg)
        end,
    EndpointWithHeaders = lists:foldl(HeaderFun, EndpointWithPath, HeaderArgs#sp_map.fields),

    RequestContentTypeMime = content_type_to_mime(RequestContentType),
    Endpoint1 =
        spectra_openapi:with_request_body(
            EndpointWithHeaders, Module, RequestBody, RequestContentTypeMime
        ),

    %% Add all responses from the responses map
    ResponseFun =
        fun(_StatusCode, ResponseSpec, EndpointAcc) ->
            #response_spec{
                status_code = ReturnCode,
                headers = ReturnHeaders,
                body = ReturnBody,
                content_type = ResponseContentType
            } = ResponseSpec,
            Description = status_code_to_description(ReturnCode),
            Response0 = spectra_openapi:response(ReturnCode, Description),
            ResponseContentTypeMime = content_type_to_mime(ResponseContentType),
            Response1 =
                spectra_openapi:response_with_body(
                    Response0, Module, ReturnBody, ResponseContentTypeMime
                ),
            ResponseWithHeaders = add_response_headers(Response1, Module, ReturnHeaders),
            spectra_openapi:add_response(EndpointAcc, ResponseWithHeaders)
        end,

    Endpoint2 = maps:fold(ResponseFun, Endpoint1, Responses),
    Endpoint2.

add_response_headers(Response, Module, #sp_map{fields = Fields}) ->
    lists:foldl(
        fun({FieldType, Name, Type}, ResponseAcc) when
            FieldType =:= map_field_exact orelse FieldType =:= map_field_assoc
        ->
            HeaderName = atom_to_binary(Name),
            HeaderSpec = #{
                required => FieldType =:= map_field_exact,
                schema => Type
            },
            spectra_openapi:response_with_header(ResponseAcc, HeaderName, Module, HeaderSpec)
        end,
        Response,
        Fields
    );
add_response_headers(Response, _Module, _Other) ->
    Response.

to_spectra_http_method(~"GET") -> get;
to_spectra_http_method(~"POST") -> post;
to_spectra_http_method(~"PUT") -> put;
to_spectra_http_method(~"DELETE") -> delete;
to_spectra_http_method(~"PATCH") -> patch;
to_spectra_http_method(~"HEAD") -> head;
to_spectra_http_method(~"OPTIONS") -> options;
to_spectra_http_method(~"TRACE") -> trace.

to_map(#sp_map{fields = Fields}) ->
    lists:foldl(
        fun({map_field_exact, Name, Type}, Acc) -> Acc#{atom_to_binary(Name) => Type} end,
        #{},
        Fields
    ).

generate_openapi_spec(MetaData, Routes) ->
    RouteEndpoints =
        lists:map(
            fun(Route) ->
                HandlerType = to_handler_type(Route),
                {Route, to_endpoint(Route, HandlerType), HandlerType}
            end,
            Routes
        ),
    Endpoints =
        lists:map(fun({_Route, Endpoint, _HandlerType}) -> Endpoint end, RouteEndpoints),
    spectra_openapi:endpoints_to_openapi(MetaData, Endpoints).

endpoints_to_file(MetaData, Routes) ->
    {ok, EndpointsJson} = generate_openapi_spec(MetaData, Routes),
    Json = json:encode(EndpointsJson),
    file:write_file("priv/openapi.json", Json).

-spec infer_content_type(spectra:sp_type()) -> content_type().
infer_content_type(#sp_simple_type{type = binary}) ->
    plain;
infer_content_type(#sp_simple_type{type = nonempty_binary}) ->
    plain;
infer_content_type(#sp_simple_type{type = atom}) ->
    plain;
infer_content_type(#sp_literal{value = V}) when is_atom(V) -> plain;
infer_content_type(#sp_union{types = Types}) ->
    case lists:all(fun(T) -> infer_content_type(T) =:= plain end, Types) of
        true -> plain;
        false -> json
    end;
infer_content_type(_) ->
    json.

-spec content_type_to_mime(content_type()) -> binary().
content_type_to_mime(plain) -> ~"text/plain";
content_type_to_mime(json) -> ~"application/json".

-spec status_code_to_description(integer()) -> binary().
status_code_to_description(200) -> ~"Success";
status_code_to_description(201) -> ~"Created";
status_code_to_description(202) -> ~"Accepted";
status_code_to_description(204) -> ~"No Content";
status_code_to_description(400) -> ~"Bad Request";
status_code_to_description(401) -> ~"Unauthorized";
status_code_to_description(403) -> ~"Forbidden";
status_code_to_description(404) -> ~"Not Found";
status_code_to_description(409) -> ~"Conflict";
status_code_to_description(422) -> ~"Unprocessable Entity";
status_code_to_description(500) -> ~"Internal Server Error";
status_code_to_description(502) -> ~"Bad Gateway";
status_code_to_description(503) -> ~"Service Unavailable";
status_code_to_description(Code) when Code >= 200, Code < 300 -> ~"Success";
status_code_to_description(Code) when Code >= 300, Code < 400 -> ~"Redirection";
status_code_to_description(Code) when Code >= 400, Code < 500 -> ~"Client Error";
status_code_to_description(Code) when Code >= 500, Code < 600 -> ~"Server Error";
status_code_to_description(_) -> ~"Unknown Status".

join_function_specs(
    MFA,
    [
        #sp_function_spec{
            args = [PathArgs, HeaderArgs, Body],
            return = ReturnType
        }
    ]
) ->
    Responses = extract_responses(ReturnType),
    #handler_type{
        mfa = MFA,
        path_args = PathArgs,
        header_args = HeaderArgs,
        request_body = Body,
        request_content_type = infer_content_type(Body),
        responses = Responses
    }.

%% Extract response specifications from return type
%% Handles both single tuple: {200, Headers, Body}
%% And union of tuples: {200, H1, B1} | {400, H2, B2} | {404, H3, B3}
-spec extract_responses(spectra:sp_type()) -> #{integer() => #response_spec{}}.
extract_responses(#sp_union{types = Types}) ->
    %% Union of multiple status codes
    lists:foldl(
        fun(TupleType, Acc) ->
            case extract_single_response(TupleType) of
                {ok, StatusCode, ResponseSpec} ->
                    maps:put(StatusCode, ResponseSpec, Acc);
                error ->
                    Acc
            end
        end,
        #{},
        Types
    );
extract_responses(TupleType) ->
    %% Single status code
    case extract_single_response(TupleType) of
        {ok, StatusCode, ResponseSpec} ->
            #{StatusCode => ResponseSpec};
        error ->
            #{}
    end.

-spec extract_single_response(spectra:sp_type()) ->
    {ok, integer(), #response_spec{}} | error.
extract_single_response(
    #sp_tuple{
        fields =
            [
                #sp_literal{value = ReturnCode},
                ReturnHeaders,
                ReturnBody
            ]
    }
) when is_integer(ReturnCode) ->
    {ok, ReturnCode, #response_spec{
        status_code = ReturnCode,
        headers = ReturnHeaders,
        body = ReturnBody,
        content_type = infer_content_type(ReturnBody)
    }};
extract_single_response(_) ->
    error.
