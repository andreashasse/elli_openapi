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


```sh
curl -s -X GET -H "Content-Type: application/json" http://localhost:3000/user/Andreas/post/2 -d '{"access":["read"],"first_name":"Andreas","last_name":"Hasselberg"}'
```
