%%% @doc Elli example callback
%%%
%%% Your callback needs to implement two functions, `handle/2' and
%%% `handle_event/3'. For every request, Elli will call your handle
%%% function with the request. When an event happens, like Elli
%%% completed a request, there was a parsing error or your handler
%%% threw an error, `handle_event/3' is called.

-module(elli_openapi_handler).
-export([handle/2, handle_event/3]).

-behaviour(elli_handler).


%%
%% ELLI REQUEST CALLBACK
%%

-spec handle(Req, Args) -> Result when
      Req    :: elli:req(),
      Args  :: elli_handler:callback_args(),
      Result :: elli_handler:result().
handle(ElliRequest, Args) ->
    io:format("elli_openapi_handler: handle ~p ~p~n", [ElliRequest, Args]),
    elli_openapi:route_call(ElliRequest).

%%
%% ELLI EVENT CALLBACKS
%%


-spec handle_event(Event, Args, Config) -> ok when
      Event  :: elli_handler:event(),
      Args   :: elli_handler:callback_args(),
      Config :: [tuple()].
handle_event(elli_startup, [], Routes) ->
    elli_openapi:setup_routes(Routes),
    ok;
handle_event(Event, Args, Config) ->
    io:format("elli_openapi_handler: handle_event ~p ~p ~p~n", [Event, Args, Config]),
    ok.
