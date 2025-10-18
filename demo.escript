#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin

main(_) ->
    Routes = [
        {<<"POST">>, <<"/api/users">>, fun elli_openapi_demo:create_user/3},
        {<<"GET">>, <<"/api/users/{userId}">>, fun elli_openapi_demo:get_user/3},
        {<<"POST">>, <<"/api/echo">>, fun elli_openapi_demo:echo_text/3}
    ],
    Port = 3000,
    ElliOpts = [
        {callback, elli_openapi_handler},
        {callback_args, Routes},
        {port, Port}
    ],

    %% Start Elli
    case elli:start_link(ElliOpts) of
        {ok, _Pid} ->
            io:format("Elli openapi is started. Access the API documentation at: http://localhost:~p/swagger~n", [Port]);
        {error, Reason} ->
            io:format("Failed to start Elli server: ~p~n~n", [Reason])
    end.
