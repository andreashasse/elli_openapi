-module(elli_openapi_handler).

-export([handle/2, handle_event/3]).

-behaviour(elli_handler).
-include_lib("elli/include/elli.hrl").

-spec handle(Req, Args) -> Result when
    Req :: elli:req(),
    Args :: elli_handler:callback_args(),
    Result :: elli_handler:result().
handle(#req{path = [~"swagger"]}, _Args) ->
    F = filename:join(code:priv_dir(elli_openapi), "swagger_ui.html"),
    {ok, [], {file, F}};
handle(#req{path = [~"api-docs"]}, _Args) ->
    F = filename:join(code:priv_dir(elli_openapi), "openapi.json"),
    {ok, [], {file, F}};
handle(ElliRequest, _Args) ->
    elli_openapi:route_call(ElliRequest).

-spec handle_event(Event, Args, Config) -> ok when
    Event :: elli_handler:event(),
    Args :: elli_handler:callback_args(),
    Config :: [tuple()].
handle_event(elli_startup, [], Routes) ->
    Modules =
        lists:map(
            fun({_, _, CallFun}) ->
                {Module, _Function, _Arity} = erlang:fun_info_mfa(CallFun),
                Module
            end,
            Routes
        ),
    lists:foreach(fun code:ensure_loaded/1, lists:usort(Modules)),
    elli_openapi:setup_routes(Routes),
    ok;
handle_event(_Event, _Args, _Config) ->
    ok.
