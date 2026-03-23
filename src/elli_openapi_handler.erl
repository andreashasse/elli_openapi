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
handle(#req{path = [~"redoc"]}, _Args) ->
    F = filename:join(code:priv_dir(elli_openapi), "redoc.html"),
    {ok, [], {file, F}};
handle(#req{path = [~"api-docs"]}, _Args) ->
    {_Mref, _MyMap, OpenApiJson} = persistent_term:get(elli_openapi),
    {ok, [{~"Content-Type", ~"application/json"}], OpenApiJson};
handle(ElliRequest, _Args) ->
    elli_openapi:route_call(ElliRequest).

-spec handle_event(Event, Args :: term(), Config) -> ok when
    Event :: elli_handler:event(),
    Config :: [tuple()] | {spectra_openapi:openapi_metadata(), [tuple()]}.
handle_event(elli_startup, [], {MetaData, Routes}) when is_map(MetaData) ->
    ensure_modules_loaded(Routes),
    elli_openapi:setup_routes(MetaData, Routes),
    ok;
handle_event(elli_startup, [], Routes) ->
    ensure_modules_loaded(Routes),
    elli_openapi:setup_routes(Routes),
    ok;
handle_event(request_complete, [Req, ReturnCode, _, _, _], _Config) ->
    io:format("Req complete: ~s ~p ~n", [elli_request:raw_path(Req), ReturnCode]),
    ok;
handle_event(_Event, _Data, _Config) ->
    ok.

ensure_modules_loaded(Routes) ->
    Modules =
        lists:map(
            fun({_, _, CallFun}) ->
                {Module, _Function, _Arity} = erlang:fun_info_mfa(CallFun),
                Module
            end,
            Routes
        ),
    lists:foreach(fun code:ensure_loaded/1, lists:usort(Modules)).
