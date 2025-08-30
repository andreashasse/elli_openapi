elli_openapi
=====

An OTP application

Build
-----

    $ rebar3 compile
```erlang
{ok, Json}=elli_openapi:pelle().
file:write_file("priv/openapi.json", iolist_to_binary(json:encode(Json))).
```
