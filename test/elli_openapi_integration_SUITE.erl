-module(elli_openapi_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    create_user_success/1,
    create_user_missing_required_field/1,
    create_user_invalid_body_format/1,
    create_user_invalid_role/1,
    get_user_success/1,
    get_user_missing_auth_header/1,
    get_user_not_found/1,
    create_user_empty_body/1,
    create_user_wrong_content_type/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        create_user_success,
        create_user_missing_required_field,
        create_user_invalid_body_format,
        create_user_invalid_role,
        create_user_empty_body,
        create_user_wrong_content_type,
        get_user_success,
        get_user_missing_auth_header,
        get_user_not_found
    ].

init_per_suite(Config) ->
    %% Trap exits so we can handle server crashes
    process_flag(trap_exit, true),

    %% Start necessary applications
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(elli),

    %% Ensure priv directory exists for openapi.json generation
    ok = filelib:ensure_dir("priv/openapi.json"),

    %% Define routes
    Routes =
        [
            {<<"POST">>, <<"/api/users">>, fun elli_openapi_demo:create_user/3},
            {<<"GET">>, <<"/api/users/{userId}">>, fun elli_openapi_demo:get_user/3}
        ],

    %% Start Elli with elli_openapi_handler
    Port = 8765,
    ElliOpts =
        [
            {callback, elli_openapi_handler},
            {callback_args, Routes},
            {port, Port}
        ],

    {ok, Pid} = elli:start_link(ElliOpts),
    unlink(Pid),
    [{elli_pid, Pid}, {port, Port} | Config].

end_per_suite(Config) ->
    Pid = ?config(elli_pid, Config),
    elli:stop(Pid),
    ok.

init_per_testcase(_TestCase, Config) ->
    Pid = ?config(elli_pid, Config),
    case erlang:is_process_alive(Pid) of
        true ->
            ct:log("Elli server process ~p is alive", [Pid]),
            Config;
        false ->
            ct:log("ERROR: Elli server process ~p is dead!", [Pid]),
            {skip, "Elli server process died"}
    end.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases - Create User
%%====================================================================

create_user_success(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users", [Port])),

    RequestBody =
        json:encode(
            #{
                <<"email">> => <<"test@example.com">>,
                <<"name">> => <<"Test User">>,
                <<"role">> => <<"admin">>
            }
        ),

    {ok, {{_, 201, _}, Headers, ResponseBody}} =
        httpc:request(
            post,
            {Url, [], "application/json", RequestBody},
            [],
            []
        ),

    %% Verify response headers
    ?assertMatch({_, _}, lists:keyfind("location", 1, Headers)),
    ?assertMatch({_, _}, lists:keyfind("etag", 1, Headers)),

    %% Verify response body
    ResponseMap = json:decode(list_to_binary(ResponseBody)),
    ?assertEqual(<<"user-123">>, maps:get(<<"id">>, ResponseMap)),
    ?assertEqual(<<"test@example.com">>, maps:get(<<"email">>, ResponseMap)),
    ?assertEqual(<<"Test User">>, maps:get(<<"name">>, ResponseMap)),
    ?assertEqual(<<"admin">>, maps:get(<<"role">>, ResponseMap)),

    ok.

create_user_missing_required_field(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users", [Port])),

    %% Missing 'email' field
    RequestBody =
        json:encode(
            #{
                <<"name">> => <<"Test User">>,
                <<"role">> => <<"user">>
            }
        ),

    {ok, {{_, StatusCode, _}, _Headers, _ResponseBody}} =
        httpc:request(
            post,
            {Url, [], "application/json", RequestBody},
            [],
            []
        ),

    %% Should return 400 Bad Request
    ?assertEqual(400, StatusCode),

    ok.

create_user_invalid_body_format(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users", [Port])),

    %% Invalid JSON
    RequestBody = "{ invalid json",

    {ok, {{_, StatusCode, _}, _Headers, _ResponseBody}} =
        httpc:request(
            post,
            {Url, [], "application/json", RequestBody},
            [],
            []
        ),

    %% Should return 400 Bad Request
    ?assertEqual(400, StatusCode),

    ok.

create_user_invalid_role(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users", [Port])),

    %% Invalid role value
    RequestBody =
        json:encode(
            #{
                <<"email">> => <<"test@example.com">>,
                <<"name">> => <<"Test User">>,
                <<"role">> => <<"superadmin">>
            }
        ),

    {ok, {{_, StatusCode, _}, _Headers, _ResponseBody}} =
        httpc:request(
            post,
            {Url, [], "application/json", RequestBody},
            [],
            []
        ),

    %% Should return 400 Bad Request
    ?assertEqual(400, StatusCode),

    ok.

create_user_empty_body(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users", [Port])),

    %% Empty body
    RequestBody = "",

    {ok, {{_, StatusCode, _}, _Headers, _ResponseBody}} =
        httpc:request(
            post,
            {Url, [], "application/json", RequestBody},
            [],
            []
        ),

    %% Should return 400 Bad Request
    ?assertEqual(400, StatusCode),

    ok.

create_user_wrong_content_type(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users", [Port])),

    RequestBody = "email=test@example.com&name=Test User",

    {ok, {{_, StatusCode, _}, _Headers, _ResponseBody}} =
        httpc:request(
            post,
            {Url, [], "application/x-www-form-urlencoded", RequestBody},
            [],
            []
        ),

    %% Should return 400 Bad Request
    ?assertEqual(400, StatusCode),

    ok.

%%====================================================================
%% Test Cases - Get User
%%====================================================================

get_user_success(Config) ->
    Port = ?config(port, Config),
    UserId = "user-456",
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users/~s", [Port, UserId])),

    {ok, {{_, 200, _}, Headers, ResponseBody}} =
        httpc:request(
            get,
            {Url, [{"authorization", "Bearer token123"}, {"content-type", "text/plain"}]},
            [],
            []
        ),

    %% Verify response headers
    ?assertMatch({_, _}, lists:keyfind("etag", 1, Headers)),
    ?assertMatch({_, _}, lists:keyfind("cache-control", 1, Headers)),

    %% Verify response body
    ResponseMap = json:decode(list_to_binary(ResponseBody)),
    ?assertEqual(list_to_binary(UserId), maps:get(~"id", ResponseMap)),
    ?assertEqual(~"user@example.com", maps:get(~"email", ResponseMap)),
    ?assertEqual(~"John Doe", maps:get(~"name", ResponseMap)),
    ?assertEqual(~"user", maps:get(~"role", ResponseMap)),

    ok.

get_user_missing_auth_header(Config) ->
    Port = ?config(port, Config),
    UserId = "user-456",
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users/~s", [Port, UserId])),

    %% No Authorization header
    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        httpc:request(
            get,
            {Url, []},
            [],
            []
        )
    ),
    ok.

get_user_not_found(Config) ->
    Port = ?config(port, Config),
    %% Request a path that doesn't match any route
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users", [Port])),

    ?assertMatch(
        {ok, {{_, 404, _}, _Headers, _ResponseBody}},
        httpc:request(
            get,
            {Url, [{"authorization", "Bearer token123"}]},
            [],
            []
        )
    ),
    ok.
