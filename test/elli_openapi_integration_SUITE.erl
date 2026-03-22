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
    list_users_no_query_params/1,
    list_users_with_query_params/1,
    list_users_invalid_query_param/1,
    openapi_spec_includes_response_headers/1,
    openapi_spec_content_types/1,
    openapi_spec_multi_status/1,
    openapi_spec_get_no_request_body/1,
    openapi_spec_query_params/1,
    openapi_spec_function_doc/1,
    swagger_ui_endpoint/1,
    redoc_endpoint/1,
    api_docs_endpoint/1
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
        list_users_no_query_params,
        list_users_with_query_params,
        list_users_invalid_query_param,
        openapi_spec_includes_response_headers,
        openapi_spec_content_types,
        openapi_spec_multi_status,
        openapi_spec_get_no_request_body,
        openapi_spec_query_params,
        openapi_spec_function_doc,
        swagger_ui_endpoint,
        redoc_endpoint,
        api_docs_endpoint
    ].

init_per_suite(Config) ->
    process_flag(trap_exit, true),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(elli),

    Routes =
        [
            {<<"POST">>, <<"/api/users">>, fun elli_openapi_demo:create_user/4},
            {<<"GET">>, <<"/api/users/{userId}">>, fun elli_openapi_demo:get_user/4},
            {<<"POST">>, <<"/api/echo">>, fun elli_openapi_demo:echo_text/4},
            {<<"POST">>, <<"/api/status">>, fun elli_openapi_demo:update_status/4},
            {<<"PUT">>, <<"/api/items/{itemId}">>, fun elli_openapi_demo:update_item/4},
            {<<"GET">>, <<"/api/users">>, fun elli_openapi_demo:list_users/4}
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
%% Helper Functions
%%====================================================================

url(Config, Path) ->
    Port = ?config(port, Config),
    lists:flatten(io_lib:format("http://localhost:~p~s", [Port, Path])).

http_get(Url) ->
    httpc:request(get, {Url, []}, [], []).

http_get(Url, Headers) ->
    httpc:request(get, {Url, Headers}, [], []).

http_post(Url, ContentType, Body) ->
    httpc:request(post, {Url, [], ContentType, Body}, [], []).

http_put(Url, ContentType, Body) ->
    httpc:request(put, {Url, [], ContentType, Body}, [], []).

%%====================================================================
%% Test Cases - Create User
%%====================================================================

create_user_success(Config) ->
    Url = url(Config, "/api/users"),

    RequestBody =
        json:encode(
            #{
                <<"email">> => <<"test@example.com">>,
                <<"name">> => <<"Test User">>,
                <<"role">> => <<"admin">>
            }
        ),

    {ok, {{_, 201, _}, Headers, ResponseBody}} = http_post(Url, "application/json", RequestBody),

    ?assertMatch({_, _}, lists:keyfind("location", 1, Headers)),
    ?assertMatch({_, _}, lists:keyfind("etag", 1, Headers)),

    ?assertMatch(
        #{
            <<"id">> := <<"user-123">>,
            <<"email">> := <<"test@example.com">>,
            <<"name">> := <<"Test User">>,
            <<"role">> := <<"admin">>
        },
        json:decode(list_to_binary(ResponseBody))
    ),

    ok.

create_user_missing_required_field(Config) ->
    Url = url(Config, "/api/users"),

    RequestBody =
        json:encode(
            #{
                <<"name">> => <<"Test User">>,
                <<"role">> => <<"user">>
            }
        ),

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        http_post(Url, "application/json", RequestBody)
    ),

    ok.

create_user_invalid_body_format(Config) ->
    Url = url(Config, "/api/users"),

    RequestBody = "{ invalid json",

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        http_post(Url, "application/json", RequestBody)
    ),

    ok.

create_user_invalid_role(Config) ->
    Url = url(Config, "/api/users"),

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
        http_post(Url, "application/json", RequestBody)
    ),

    ok.

create_user_empty_body(Config) ->
    Url = url(Config, "/api/users"),

    RequestBody = "",

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        http_post(Url, "application/json", RequestBody)
    ),

    ok.

create_user_wrong_content_type(Config) ->
    Url = url(Config, "/api/users"),

    RequestBody = "email=test@example.com&name=Test User",

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        http_post(Url, "application/x-www-form-urlencoded", RequestBody)
    ),

    ok.

%%====================================================================
%% Test Cases - Get User
%%====================================================================

get_user_success(Config) ->
    UserId = "user-456",
    Url = url(Config, "/api/users/" ++ UserId),

    {ok, {{_, 200, _}, Headers, ResponseBody}} =
        http_get(Url, [{"authorization", "Bearer token123"}, {"content-type", "text/plain"}]),

    ?assertMatch({_, _}, lists:keyfind("etag", 1, Headers)),
    ?assertMatch({_, _}, lists:keyfind("cache-control", 1, Headers)),

    ?assertMatch(
        #{
            ~"id" := ~"user-456",
            ~"email" := ~"user@example.com",
            ~"name" := ~"John Doe",
            ~"role" := ~"user"
        },
        json:decode(list_to_binary(ResponseBody))
    ),

    ok.

get_user_missing_auth_header(Config) ->
    UserId = "user-456",
    Url = url(Config, "/api/users/" ++ UserId),

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        http_get(Url)
    ),
    ok.

get_user_not_found(Config) ->
    Url = url(Config, "/api/nonexistent"),

    ?assertMatch(
        {ok, {{_, 404, _}, _Headers, _ResponseBody}},
        http_get(Url, [{"authorization", "Bearer token123"}])
    ),
    ok.

%%====================================================================
%% Test Cases - Update Status
%%====================================================================

update_status_success(Config) ->
    Url = url(Config, "/api/status"),

    RequestBody = "running",

    ?assertMatch(
        {ok, {{_, 200, _}, _Headers, "running"}},
        http_post(Url, "text/plain", RequestBody)
    ),

    ok.

update_status_invalid_value(Config) ->
    Url = url(Config, "/api/status"),

    RequestBody = "invalid_status",

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        http_post(Url, "text/plain", RequestBody)
    ),

    ok.

update_status_wrong_content_type(Config) ->
    Url = url(Config, "/api/status"),

    RequestBody = json:encode(#{<<"status">> => <<"running">>}),

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        http_post(Url, "application/json", RequestBody)
    ),

    ok.

%%====================================================================
%% Test Cases - Multi-Status Update Item
%%====================================================================

update_item_success_200(Config) ->
    ItemId = "item-123",
    Url = url(Config, "/api/items/" ++ ItemId),

    RequestBody = json:encode(#{<<"name">> => <<"Updated Item">>, <<"version">> => 5}),

    {ok, {{_, 200, _}, Headers, ResponseBody}} = http_put(Url, "application/json", RequestBody),

    ?assertMatch({_, _}, lists:keyfind("etag", 1, Headers)),

    ?assertMatch(
        #{~"id" := ~"item-123", ~"name" := ~"Updated Item", ~"version" := 5},
        json:decode(list_to_binary(ResponseBody))
    ),

    ok.

update_item_not_found_404(Config) ->
    ItemId = "item-notfound",
    Url = url(Config, "/api/items/" ++ ItemId),

    RequestBody = json:encode(#{<<"name">> => <<"Any Name">>, <<"version">> => 1}),

    {ok, {{_, 404, _}, _Headers, ResponseBody}} = http_put(Url, "application/json", RequestBody),

    ?assertMatch(
        #{~"message" := ~"Item not found", ~"code" := ~"ITEM_NOT_FOUND"},
        json:decode(list_to_binary(ResponseBody))
    ),

    ok.

update_item_invalid_version_400(Config) ->
    ItemId = "item-123",
    Url = url(Config, "/api/items/" ++ ItemId),

    RequestBody = json:encode(#{<<"name">> => <<"Any Name">>, <<"version">> => -1}),

    {ok, {{_, 400, _}, _Headers, ResponseBody}} = http_put(Url, "application/json", RequestBody),

    ?assertMatch(
        #{~"message" := ~"Version must be non-negative", ~"code" := ~"INVALID_VERSION"},
        json:decode(list_to_binary(ResponseBody))
    ),

    ok.

update_item_conflict_409(Config) ->
    ItemId = "item-conflict",
    Url = url(Config, "/api/items/" ++ ItemId),

    RequestBody = json:encode(#{<<"name">> => <<"Any Name">>, <<"version">> => 5}),

    {ok, {{_, 409, _}, _Headers, ResponseBody}} = http_put(Url, "application/json", RequestBody),

    ?assertMatch(
        #{~"message" := ~"Version conflict detected", ~"code" := ~"VERSION_CONFLICT"},
        json:decode(list_to_binary(ResponseBody))
    ),

    ok.

%%====================================================================
%% Test Cases - OpenAPI Spec
%%====================================================================

openapi_spec_includes_response_headers(_Config) ->
    Routes =
        [
            {<<"POST">>, <<"/api/users">>, fun elli_openapi_demo:create_user/4},
            {<<"GET">>, <<"/api/users/{userId}">>, fun elli_openapi_demo:get_user/4},
            {<<"POST">>, <<"/api/status">>, fun elli_openapi_demo:update_status/4}
        ],

    MetaData = #{title => ~"Test API", version => ~"1.0.0"},
    {ok, Spec} = elli_openapi:generate_openapi_spec(MetaData, Routes),

    ?assertMatch(
        #{
            <<"paths">> := #{
                <<"/api/users">> := #{
                    <<"post">> := #{
                        <<"responses">> := #{
                            <<"201">> := #{
                                <<"headers">> := #{
                                    <<"Location">> := #{<<"schema">> := #{type := <<"string">>}},
                                    <<"ETag">> := #{<<"schema">> := #{type := <<"string">>}}
                                }
                            }
                        }
                    }
                },
                <<"/api/users/{userId}">> := #{
                    <<"get">> := #{
                        <<"responses">> := #{
                            <<"200">> := #{
                                <<"headers">> := #{
                                    <<"ETag">> := #{<<"schema">> := #{type := <<"string">>}},
                                    <<"Cache-Control">> := #{
                                        <<"schema">> := #{type := <<"string">>}
                                    }
                                }
                            }
                        }
                    }
                },
                <<"/api/status">> := #{
                    <<"post">> := #{
                        <<"requestBody">> := #{<<"content">> := #{<<"text/plain">> := _}},
                        <<"responses">> := #{
                            <<"200">> := #{
                                <<"content">> := #{<<"text/plain">> := _}
                            }
                        }
                    }
                }
            }
        },
        Spec
    ),

    ok.

openapi_spec_content_types(_Config) ->
    Routes =
        [
            {<<"POST">>, <<"/api/users">>, fun elli_openapi_demo:create_user/4},
            {<<"GET">>, <<"/api/users/{userId}">>, fun elli_openapi_demo:get_user/4},
            {<<"POST">>, <<"/api/echo">>, fun elli_openapi_demo:echo_text/4},
            {<<"POST">>, <<"/api/status">>, fun elli_openapi_demo:update_status/4}
        ],

    MetaData = #{title => ~"Test API", version => ~"1.0.0"},
    {ok, Spec} = elli_openapi:generate_openapi_spec(MetaData, Routes),

    #{
        <<"paths">> := #{
            <<"/api/echo">> := #{
                <<"post">> := #{
                    <<"requestBody">> := #{<<"content">> := EchoReqContent},
                    <<"responses">> := #{<<"200">> := #{<<"content">> := EchoRespContent}}
                }
            },
            <<"/api/status">> := #{
                <<"post">> := #{
                    <<"requestBody">> := #{<<"content">> := StatusReqContent},
                    <<"responses">> := #{<<"200">> := #{<<"content">> := StatusRespContent}}
                }
            },
            <<"/api/users">> := #{
                <<"post">> := #{
                    <<"requestBody">> := #{<<"content">> := UsersReqContent},
                    <<"responses">> := #{<<"201">> := #{<<"content">> := UsersRespContent}}
                }
            }
        }
    } = Spec,

    ?assertEqual([<<"text/plain">>], maps:keys(EchoReqContent)),
    ?assertEqual([<<"text/plain">>], maps:keys(EchoRespContent)),
    ?assertEqual([<<"text/plain">>], maps:keys(StatusReqContent)),
    ?assertEqual([<<"text/plain">>], maps:keys(StatusRespContent)),
    ?assertEqual([<<"application/json">>], maps:keys(UsersReqContent)),
    ?assertEqual([<<"application/json">>], maps:keys(UsersRespContent)),

    ok.

openapi_spec_multi_status(_Config) ->
    Routes = [{<<"PUT">>, <<"/api/items/{itemId}">>, fun elli_openapi_demo:update_item/4}],

    MetaData = #{title => ~"Test API", version => ~"1.0.0"},
    {ok, Spec} = elli_openapi:generate_openapi_spec(MetaData, Routes),

    %% Verify all 4 status codes with proper descriptions and content types
    ?assertMatch(
        #{
            <<"paths">> := #{
                <<"/api/items/{itemId}">> := #{
                    <<"put">> := #{
                        <<"responses">> := #{
                            <<"200">> := #{
                                <<"headers">> := #{<<"ETag">> := _},
                                <<"description">> := ~"Success",
                                <<"content">> := #{<<"application/json">> := _}
                            },
                            <<"400">> := #{
                                <<"description">> := ~"Bad Request",
                                <<"content">> := #{<<"application/json">> := _}
                            },
                            <<"404">> := #{
                                <<"description">> := ~"Not Found",
                                <<"content">> := #{<<"application/json">> := _}
                            },
                            <<"409">> := #{
                                <<"description">> := ~"Conflict",
                                <<"content">> := #{<<"application/json">> := _}
                            }
                        }
                    }
                }
            }
        },
        Spec
    ),

    ok.

openapi_spec_get_no_request_body(_Config) ->
    %% Test that GET requests do not generate requestBody in OpenAPI spec
    Routes = [
        {<<"GET">>, <<"/api/users/{userId}">>, fun elli_openapi_demo:get_user/4},
        {<<"POST">>, <<"/api/users">>, fun elli_openapi_demo:create_user/4}
    ],

    MetaData = #{title => ~"Test API", version => ~"1.0.0"},
    {ok, Spec} = elli_openapi:generate_openapi_spec(MetaData, Routes),

    #{<<"paths">> := Paths} = Spec,

    %% Verify GET request has no requestBody
    #{<<"/api/users/{userId}">> := UserPath} = Paths,
    #{<<"get">> := GetEndpoint} = UserPath,
    ?assertNot(maps:is_key(<<"requestBody">>, GetEndpoint)),

    %% Verify POST request has requestBody
    #{<<"/api/users">> := UsersPath} = Paths,
    #{<<"post">> := PostEndpoint} = UsersPath,
    ?assert(maps:is_key(<<"requestBody">>, PostEndpoint)),

    ok.

%%====================================================================
%% Test Cases - Query Parameters
%%====================================================================

list_users_no_query_params(Config) ->
    Url = url(Config, "/api/users"),

    ?assertMatch(
        {ok, {{_, 200, _}, _Headers, _ResponseBody}},
        http_get(Url)
    ),
    ok.

list_users_with_query_params(Config) ->
    Url = url(Config, "/api/users?page=2&per_page=5"),

    ?assertMatch(
        {ok, {{_, 200, _}, _Headers, _ResponseBody}},
        http_get(Url)
    ),
    ok.

list_users_invalid_query_param(Config) ->
    Url = url(Config, "/api/users?page=notanumber"),

    ?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        http_get(Url)
    ),
    ok.

%%====================================================================
%% Test Cases - OpenAPI Spec: Query Params and Function Docs
%%====================================================================

openapi_spec_query_params(_Config) ->
    Routes = [{<<"GET">>, <<"/api/users">>, fun elli_openapi_demo:list_users/4}],

    MetaData = #{title => ~"Test API", version => ~"1.0.0"},
    {ok, Spec} = elli_openapi:generate_openapi_spec(MetaData, Routes),

    #{<<"paths">> := #{<<"/api/users">> := #{<<"get">> := GetEndpoint}}} = Spec,
    #{<<"parameters">> := Params} = GetEndpoint,

    ParamNames = [maps:get(<<"name">>, P) || P <- Params],
    ?assert(lists:member(<<"page">>, ParamNames)),
    ?assert(lists:member(<<"per_page">>, ParamNames)),

    [PageParam] = [P || P <- Params, maps:get(<<"name">>, P) =:= <<"page">>],
    ?assertEqual(<<"query">>, maps:get(<<"in">>, PageParam)),
    ?assertEqual(false, maps:get(<<"required">>, PageParam)),

    ok.

openapi_spec_function_doc(_Config) ->
    Routes = [
        {<<"POST">>, <<"/api/users">>, fun elli_openapi_demo:create_user/4},
        {<<"GET">>, <<"/api/users">>, fun elli_openapi_demo:list_users/4}
    ],

    MetaData = #{title => ~"Test API", version => ~"1.0.0"},
    {ok, Spec} = elli_openapi:generate_openapi_spec(MetaData, Routes),

    ?assertMatch(
        #{
            <<"paths">> := #{
                <<"/api/users">> := #{
                    <<"post">> := #{<<"summary">> := <<"Create a new user">>},
                    <<"get">> := #{<<"summary">> := <<"List users">>}
                }
            }
        },
        Spec
    ),
    ok.

%%====================================================================
%% Test Cases - Documentation Endpoints
%%====================================================================

swagger_ui_endpoint(Config) ->
    ?assertMatch(
        {ok, {{_, 200, _}, _Headers, _ResponseBody}},
        http_get(url(Config, "/swagger"))
    ),
    ok.

redoc_endpoint(Config) ->
    ?assertMatch(
        {ok, {{_, 200, _}, _Headers, _ResponseBody}},
        http_get(url(Config, "/redoc"))
    ),
    ok.

api_docs_endpoint(Config) ->
    ?assertMatch(
        {ok, {{_, 200, _}, _Headers, _ResponseBody}},
        http_get(url(Config, "/api-docs"))
    ),
    ok.
