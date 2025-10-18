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
    create_user_wrong_content_type/1,
    update_status_success/1,
    update_status_invalid_value/1,
    update_status_wrong_content_type/1,
    update_item_success_200/1,
    update_item_not_found_404/1,
    update_item_invalid_version_400/1,
    update_item_conflict_409/1,
    openapi_spec_includes_response_headers/1,
    openapi_spec_content_types/1,
    openapi_spec_multi_status/1
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
        get_user_not_found,
        update_status_success,
        update_status_invalid_value,
        update_status_wrong_content_type,
        update_item_success_200,
        update_item_not_found_404,
        update_item_invalid_version_400,
        update_item_conflict_409,
        openapi_spec_includes_response_headers,
        openapi_spec_content_types,
        openapi_spec_multi_status
    ].

init_per_suite(Config) ->
    process_flag(trap_exit, true),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(elli),
    ok = filelib:ensure_dir("priv/openapi.json"),

    Routes =
        [
            {<<"POST">>, <<"/api/users">>, fun elli_openapi_demo:create_user/3},
            {<<"GET">>, <<"/api/users/{userId}">>, fun elli_openapi_demo:get_user/3},
            {<<"POST">>, <<"/api/echo">>, fun elli_openapi_demo:echo_text/3},
            {<<"POST">>, <<"/api/status">>, fun elli_openapi_demo:update_status/3},
            {<<"PUT">>, <<"/api/items/{itemId}">>, fun elli_openapi_demo:update_item/3}
        ],

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
    Config.

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

    Result = httpc:request(
        post,
        {Url, [], "application/json", RequestBody},
        [],
        []
    ),
    ?assertMatch({ok, {{_, 201, _}, _Headers, _ResponseBody}}, Result),
    {ok, {_, Headers, ResponseBody}} = Result,

    ?assertMatch({_, _}, lists:keyfind("location", 1, Headers)),
    ?assertMatch({_, _}, lists:keyfind("etag", 1, Headers)),

    ResponseMap = json:decode(list_to_binary(ResponseBody)),
    ?assertEqual(<<"user-123">>, maps:get(<<"id">>, ResponseMap)),
    ?assertEqual(<<"test@example.com">>, maps:get(<<"email">>, ResponseMap)),
    ?assertEqual(<<"Test User">>, maps:get(<<"name">>, ResponseMap)),
    ?assertEqual(<<"admin">>, maps:get(<<"role">>, ResponseMap)),

    ok.

create_user_missing_required_field(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users", [Port])),

    RequestBody =
        json:encode(
            #{
                <<"name">> => <<"Test User">>,
                <<"role">> => <<"user">>
            }
        ),

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        httpc:request(
            post,
            {Url, [], "application/json", RequestBody},
            [],
            []
        )
    ),

    ok.

create_user_invalid_body_format(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users", [Port])),

    RequestBody = "{ invalid json",

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        httpc:request(
            post,
            {Url, [], "application/json", RequestBody},
            [],
            []
        )
    ),

    ok.

create_user_invalid_role(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users", [Port])),

    RequestBody =
        json:encode(
            #{
                <<"email">> => <<"test@example.com">>,
                <<"name">> => <<"Test User">>,
                <<"role">> => <<"superadmin">>
            }
        ),

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        httpc:request(
            post,
            {Url, [], "application/json", RequestBody},
            [],
            []
        )
    ),

    ok.

create_user_empty_body(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users", [Port])),

    RequestBody = "",

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        httpc:request(
            post,
            {Url, [], "application/json", RequestBody},
            [],
            []
        )
    ),

    ok.

create_user_wrong_content_type(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users", [Port])),

    RequestBody = "email=test@example.com&name=Test User",

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        httpc:request(
            post,
            {Url, [], "application/x-www-form-urlencoded", RequestBody},
            [],
            []
        )
    ),

    ok.

%%====================================================================
%% Test Cases - Get User
%%====================================================================

get_user_success(Config) ->
    Port = ?config(port, Config),
    UserId = "user-456",
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/users/~s", [Port, UserId])),

    Result = httpc:request(
        get,
        {Url, [{"authorization", "Bearer token123"}, {"content-type", "text/plain"}]},
        [],
        []
    ),
    ?assertMatch({ok, {{_, 200, _}, _Headers, _ResponseBody}}, Result),
    {ok, {_, Headers, ResponseBody}} = Result,

    ?assertMatch({_, _}, lists:keyfind("etag", 1, Headers)),
    ?assertMatch({_, _}, lists:keyfind("cache-control", 1, Headers)),

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

%%====================================================================
%% Test Cases - Update Status
%%====================================================================

update_status_success(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/status", [Port])),

    RequestBody = "running",

    ?assertMatch(
        {ok, {{_, 200, _}, _Headers, "running"}},
        httpc:request(
            post,
            {Url, [], "text/plain", RequestBody},
            [],
            []
        )
    ),

    ok.

update_status_invalid_value(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/status", [Port])),

    RequestBody = "invalid_status",

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        httpc:request(
            post,
            {Url, [], "text/plain", RequestBody},
            [],
            []
        )
    ),

    ok.

update_status_wrong_content_type(Config) ->
    Port = ?config(port, Config),
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/status", [Port])),

    RequestBody = json:encode(#{<<"status">> => <<"running">>}),

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        httpc:request(
            post,
            {Url, [], "application/json", RequestBody},
            [],
            []
        )
    ),

    ok.

%%====================================================================
%% Test Cases - Multi-Status Update Item
%%====================================================================

update_item_success_200(Config) ->
    Port = ?config(port, Config),
    ItemId = "item-123",
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/items/~s", [Port, ItemId])),

    RequestBody = json:encode(#{<<"name">> => <<"Updated Item">>, <<"version">> => 5}),

    Result = httpc:request(
        put,
        {Url, [], "application/json", RequestBody},
        [],
        []
    ),
    ?assertMatch({ok, {{_, 200, _}, _Headers, _ResponseBody}}, Result),
    {ok, {_, Headers, ResponseBody}} = Result,

    ?assertMatch({_, _}, lists:keyfind("etag", 1, Headers)),

    ResponseMap = json:decode(list_to_binary(ResponseBody)),
    ?assertEqual(list_to_binary(ItemId), maps:get(~"id", ResponseMap)),
    ?assertEqual(~"Updated Item", maps:get(~"name", ResponseMap)),
    ?assertEqual(5, maps:get(~"version", ResponseMap)),

    ok.

update_item_not_found_404(Config) ->
    Port = ?config(port, Config),
    ItemId = "item-notfound",
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/items/~s", [Port, ItemId])),

    RequestBody = json:encode(#{<<"name">> => <<"Any Name">>, <<"version">> => 1}),

    Result = httpc:request(
        put,
        {Url, [], "application/json", RequestBody},
        [],
        []
    ),
    ?assertMatch({ok, {{_, 404, _}, _Headers, _ResponseBody}}, Result),
    {ok, {_, _Headers, ResponseBody}} = Result,

    ResponseMap = json:decode(list_to_binary(ResponseBody)),
    ?assertEqual(~"Item not found", maps:get(~"message", ResponseMap)),
    ?assertEqual(~"ITEM_NOT_FOUND", maps:get(~"code", ResponseMap)),

    ok.

update_item_invalid_version_400(Config) ->
    Port = ?config(port, Config),
    ItemId = "item-123",
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/items/~s", [Port, ItemId])),

    RequestBody = json:encode(#{<<"name">> => <<"Any Name">>, <<"version">> => -1}),

    Result = httpc:request(
        put,
        {Url, [], "application/json", RequestBody},
        [],
        []
    ),
    ?assertMatch({ok, {{_, 400, _}, _Headers, _ResponseBody}}, Result),
    {ok, {_, _Headers, ResponseBody}} = Result,

    ResponseMap = json:decode(list_to_binary(ResponseBody)),
    ?assertEqual(~"Version must be non-negative", maps:get(~"message", ResponseMap)),
    ?assertEqual(~"INVALID_VERSION", maps:get(~"code", ResponseMap)),

    ok.

update_item_conflict_409(Config) ->
    Port = ?config(port, Config),
    ItemId = "item-conflict",
    Url = lists:flatten(io_lib:format("http://localhost:~p/api/items/~s", [Port, ItemId])),

    RequestBody = json:encode(#{<<"name">> => <<"Any Name">>, <<"version">> => 5}),

    Result = httpc:request(
        put,
        {Url, [], "application/json", RequestBody},
        [],
        []
    ),
    ?assertMatch({ok, {{_, 409, _}, _Headers, _ResponseBody}}, Result),
    {ok, {_, _Headers, ResponseBody}} = Result,

    ResponseMap = json:decode(list_to_binary(ResponseBody)),
    ?assertEqual(~"Version conflict detected", maps:get(~"message", ResponseMap)),
    ?assertEqual(~"VERSION_CONFLICT", maps:get(~"code", ResponseMap)),

    ok.

%%====================================================================
%% Test Cases - OpenAPI Spec
%%====================================================================

openapi_spec_includes_response_headers(_Config) ->
    Routes =
        [
            {<<"POST">>, <<"/api/users">>, fun elli_openapi_demo:create_user/3},
            {<<"GET">>, <<"/api/users/{userId}">>, fun elli_openapi_demo:get_user/3},
            {<<"POST">>, <<"/api/status">>, fun elli_openapi_demo:update_status/3}
        ],

    MetaData = #{title => ~"Test API", version => ~"1.0.0"},
    {ok, Spec} = elli_openapi:generate_openapi_spec(MetaData, Routes),

    #{
        paths := #{
            <<"/api/users">> := #{
                post := #{
                    responses := #{
                        <<"201">> := #{
                            headers := UsersHeaders
                        }
                    }
                }
            }
        }
    } = Spec,

    ?assertMatch(
        #{<<"Location">> := #{schema := #{type := <<"string">>}}},
        UsersHeaders
    ),
    ?assertMatch(
        #{<<"ETag">> := #{schema := #{type := <<"string">>}}},
        UsersHeaders
    ),

    #{
        paths := #{
            <<"/api/users/{userId}">> := #{
                get := #{
                    responses := #{
                        <<"200">> := #{
                            headers := GetUserHeaders
                        }
                    }
                }
            }
        }
    } = Spec,

    ?assertMatch(
        #{<<"ETag">> := #{schema := #{type := <<"string">>}}},
        GetUserHeaders
    ),
    ?assertMatch(
        #{<<"Cache-Control">> := #{schema := #{type := <<"string">>}}},
        GetUserHeaders
    ),

    #{
        paths := #{
            <<"/api/status">> := #{
                post := #{
                    requestBody := #{content := StatusReqContent},
                    responses := #{
                        <<"200">> := #{
                            content := StatusRespContent
                        }
                    }
                }
            }
        }
    } = Spec,

    ?assertMatch(#{<<"text/plain">> := _}, StatusReqContent),
    ?assertMatch(#{<<"text/plain">> := _}, StatusRespContent),

    ok.

openapi_spec_content_types(_Config) ->
    Routes =
        [
            {<<"POST">>, <<"/api/users">>, fun elli_openapi_demo:create_user/3},
            {<<"GET">>, <<"/api/users/{userId}">>, fun elli_openapi_demo:get_user/3},
            {<<"POST">>, <<"/api/echo">>, fun elli_openapi_demo:echo_text/3},
            {<<"POST">>, <<"/api/status">>, fun elli_openapi_demo:update_status/3}
        ],

    MetaData = #{title => ~"Test API", version => ~"1.0.0"},
    {ok, Spec} = elli_openapi:generate_openapi_spec(MetaData, Routes),

    #{
        paths := #{
            <<"/api/echo">> := #{
                post := #{
                    requestBody := #{content := EchoReqContent},
                    responses := #{
                        <<"200">> := #{content := EchoRespContent}
                    }
                }
            }
        }
    } = Spec,

    ?assertEqual([<<"text/plain">>], maps:keys(EchoReqContent)),
    ?assertEqual([<<"text/plain">>], maps:keys(EchoRespContent)),

    #{
        paths := #{
            <<"/api/status">> := #{
                post := #{
                    requestBody := #{content := StatusReqContent},
                    responses := #{
                        <<"200">> := #{content := StatusRespContent}
                    }
                }
            }
        }
    } = Spec,

    ?assertEqual([<<"text/plain">>], maps:keys(StatusReqContent)),
    ?assertEqual([<<"text/plain">>], maps:keys(StatusRespContent)),

    #{
        paths := #{
            <<"/api/users">> := #{
                post := #{
                    requestBody := #{content := UsersReqContent},
                    responses := #{
                        <<"201">> := #{content := UsersRespContent}
                    }
                }
            }
        }
    } = Spec,

    ?assertEqual([<<"application/json">>], maps:keys(UsersReqContent)),
    ?assertEqual([<<"application/json">>], maps:keys(UsersRespContent)),

    ok.

openapi_spec_multi_status(_Config) ->
    Routes = [{<<"PUT">>, <<"/api/items/{itemId}">>, fun elli_openapi_demo:update_item/3}],

    MetaData = #{title => ~"Test API", version => ~"1.0.0"},
    {ok, Spec} = elli_openapi:generate_openapi_spec(MetaData, Routes),

    #{
        paths := #{
            <<"/api/items/{itemId}">> := #{
                put := #{
                    responses := Responses
                }
            }
        }
    } = Spec,

    %% Verify all 4 status codes are present
    ?assertMatch(#{<<"200">> := _}, Responses),
    ?assertMatch(#{<<"400">> := _}, Responses),
    ?assertMatch(#{<<"404">> := _}, Responses),
    ?assertMatch(#{<<"409">> := _}, Responses),

    %% Verify 200 response has ETag header and item body
    #{<<"200">> := Response200} = Responses,
    ?assertMatch(#{headers := #{<<"ETag">> := _}}, Response200),
    ?assertMatch(#{content := #{<<"application/json">> := _}}, Response200),

    %% Verify error responses (400, 404, 409) have error_response body
    #{<<"400">> := Response400} = Responses,
    ?assertMatch(#{content := #{<<"application/json">> := _}}, Response400),

    #{<<"404">> := Response404} = Responses,
    ?assertMatch(#{content := #{<<"application/json">> := _}}, Response404),

    #{<<"409">> := Response409} = Responses,
    ?assertMatch(#{content := #{<<"application/json">> := _}}, Response409),

    ok.
