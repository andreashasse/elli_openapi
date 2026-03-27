-module(demo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = demo_sup:start_link(),
    io:format("Demo started. API docs at http://localhost:3000/swagger~n"),
    {ok, Pid}.

stop(_State) ->
    ok.
