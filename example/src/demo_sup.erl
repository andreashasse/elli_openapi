-module(demo_sup).

-behaviour(supervisor).

-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ElliOpts = [
        {callback, elli_openapi_handler},
        {callback_args, routes()},
        {port, 3000}
    ],
    Children = [
        #{id => elli, start => {elli, start_link, [ElliOpts]}, restart => permanent}
    ],
    {ok, {#{strategy => one_for_one}, Children}}.

routes() ->
    [
        {<<"POST">>, <<"/api/users">>, fun elli_openapi_demo:create_user/4},
        {<<"GET">>, <<"/api/users/{userId}">>, fun elli_openapi_demo:get_user/4},
        {<<"POST">>, <<"/api/echo">>, fun elli_openapi_demo:echo_text/4},
        {<<"POST">>, <<"/api/status">>, fun elli_openapi_demo:update_status/4},
        {<<"PUT">>, <<"/api/items/{itemId}">>, fun elli_openapi_demo:update_item/4},
        {<<"GET">>, <<"/api/users">>, fun elli_openapi_demo:list_users/4},
        {<<"GET">>, <<"/api/search">>, fun elli_openapi_demo:search_users/4}
    ].
