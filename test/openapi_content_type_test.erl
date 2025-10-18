-module(openapi_content_type_test).
-export([test/0]).

test() ->
    application:ensure_all_started(inets),
    Routes = [
        {<<"POST">>, <<"/api/users">>, fun elli_openapi_demo:create_user/3},
        {<<"GET">>, <<"/api/users/{userId}">>, fun elli_openapi_demo:get_user/3},
        {<<"POST">>, <<"/api/echo">>, fun elli_openapi_demo:echo_text/3}
    ],
    ElliOpts = [
        {callback, elli_openapi_handler},
        {callback_args, Routes},
        {port, 3001}
    ],
    {ok, Pid} = elli:start_link(ElliOpts),
    unlink(Pid),
    timer:sleep(500),

    {ok, {{_, 200, _}, _, Body}} = httpc:request("http://localhost:3001/openapi.json"),
    {ok, Json} = json:decode(iolist_to_binary(Body)),

    %% Check /api/echo endpoint (should be text/plain)
    #{<<"paths">> := Paths} = Json,
    #{<<"/api/echo">> := EchoPath} = Paths,
    #{<<"post">> := EchoPost} = EchoPath,

    %% Check request body content-type
    #{<<"requestBody">> := RequestBody} = EchoPost,
    #{<<"content">> := ReqContent} = RequestBody,
    ReqContentTypes = maps:keys(ReqContent),
    io:format("Request body content types for /api/echo: ~p~n", [ReqContentTypes]),

    %% Check response content-type
    #{<<"responses">> := Responses} = EchoPost,
    #{<<"200">> := Response200} = Responses,
    #{<<"content">> := RespContent} = Response200,
    RespContentTypes = maps:keys(RespContent),
    io:format("Response content types for /api/echo: ~p~n", [RespContentTypes]),

    %% Check /api/users endpoint (should be JSON)
    #{<<"/api/users">> := UsersPath} = Paths,
    #{<<"post">> := UsersPost} = UsersPath,
    #{<<"requestBody">> := UsersRequestBody} = UsersPost,
    #{<<"content">> := UsersReqContent} = UsersRequestBody,
    UsersReqContentTypes = maps:keys(UsersReqContent),
    io:format("Request body content types for /api/users: ~p~n", [UsersReqContentTypes]),

    %% Verify expectations
    case {ReqContentTypes, RespContentTypes, UsersReqContentTypes} of
        {[<<"text/plain">>], [<<"text/plain">>], [<<"application/json">>]} ->
            io:format("SUCCESS: Content-types are correctly detected!~n"),
            io:format("  - /api/echo uses text/plain for binary() types~n"),
            io:format("  - /api/users uses application/json for structured types~n");
        _ ->
            io:format("UNEXPECTED: Content-types don't match expected values~n")
    end,

    elli:stop(Pid),
    ok.
