- After each change run `make build-test`
- Use the new binary syntax in erlang. ~"Binary Value" is the new way of writing <<"Binary Value">>.
- Use nested decomposition in tests. Eg:

#{<<"paths">> := #{<<"/api/echo">> := #{<<"post">> := EchoPost}}} = Json.

instead of

#{<<"paths">> := Paths} = Json, #{<<"/api/echo">> := EchoPath} = Paths, #{<<"post">> := EchoPost} = EchoPath,
- Do ?assert...(pattern, value) as "early" as possible.
Eg, Instead of:

{ok, {{_, StatusCode, _}, _Headers, _ResponseBody}} =
        httpc:request(
            post,
            {Url, [], "text/plain", RequestBody},
            [],
            []
        ),
?assertEqual(400, StatusCode),

Do:

?assertMatch(
        {ok, {{_, 400, _}, _Headers, _ResponseBody}},
        httpc:request(
            get,
            {Url, []},
            [],
            []
        )
    ),
- Assert "earlier", instead of:
UsersReqContentTypes = maps:keys(UsersReqContent),
?assertEqual([<<"application/json">>], UsersReqContentTypes),

Do ?assertEqual([<<"application/json">>], maps:keys(UsersReqContent),