%%%-------------------------------------------------------------------
%% @doc elli_openapi public API
%% @end
%%%-------------------------------------------------------------------

-module(elli_openapi_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    elli_openapi_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
