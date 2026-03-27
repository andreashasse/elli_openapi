-module(user_handler_SUITE).

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
    get_user_success/1,
    get_user_not_found/1,
    create_user_success/1,
    create_user_missing_field/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        get_user_success,
        get_user_not_found,
        create_user_success,
        create_user_missing_field
    ].

init_per_suite(Config) ->
    process_flag(trap_exit, true),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(elli),

    Routes = [
        {<<"GET">>, <<"/api/users/{userId}">>, fun user_handler:get_user/4},
        {<<"POST">>, <<"/api/users">>, fun user_handler:create_user/4}
    ],

    Port = 8766,
    ElliOpts = [
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
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

url(Config, Path) ->
    Port = ?config(port, Config),
    lists:flatten(io_lib:format("http://localhost:~p~s", [Port, Path])).

http_get(Url) ->
    httpc:request(get, {Url, []}, [], []).

http_post(Url, ContentType, Body) ->
    httpc:request(post, {Url, [], ContentType, Body}, [], []).

%%====================================================================
%% Test Cases
%%====================================================================

get_user_success(Config) ->
    {ok, {{_, 200, _}, _Headers, ResponseBody}} = http_get(url(Config, "/api/users/123")),

    ?assertMatch(
        #{<<"id">> := <<"123">>, <<"name">> := <<"Alice">>, <<"role">> := <<"user">>},
        json:decode(list_to_binary(ResponseBody))
    ).

get_user_not_found(Config) ->
    ?assertMatch(
        {ok, {{_, 404, _}, _Headers, _Body}},
        http_get(url(Config, "/api/users/999"))
    ).

create_user_success(Config) ->
    RequestBody = json:encode(#{
        <<"id">> => <<"user-42">>,
        <<"name">> => <<"Bob">>,
        <<"role">> => <<"admin">>
    }),

    {ok, {{_, 201, _}, Headers, ResponseBody}} =
        http_post(url(Config, "/api/users"), "application/json", RequestBody),

    ?assertMatch({_, _}, lists:keyfind("location", 1, Headers)),
    ?assertMatch(
        #{<<"id">> := <<"user-42">>, <<"name">> := <<"Bob">>, <<"role">> := <<"admin">>},
        json:decode(list_to_binary(ResponseBody))
    ).

create_user_missing_field(Config) ->
    RequestBody = json:encode(#{<<"name">> => <<"Bob">>}),

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _Body}},
        http_post(url(Config, "/api/users"), "application/json", RequestBody)
    ).
