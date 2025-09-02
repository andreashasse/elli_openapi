-module(elli_openapi_demo).

-export([endpoint/1, endpoint2/1]).

-compile(nowarn_unused_type).

-record(user,
        {first_name :: string(),
         last_name :: string(),
         age :: integer() | undefined,
         access :: [read | write | delete]}).

-spec endpoint(#{name => string(), shirt_size => small | medium | large}) ->
                  {200, [], #{something => string()}}.
endpoint(#{name := Name, shirt_size := Size}) ->
    {200,
     [],
     #{something => "Hello " ++ Name ++ ", your shirt size is " ++ atom_to_list(Size) ++ "!"}}.

-spec endpoint2(#user{}) -> {200, [], string()}.
endpoint2(#user{access = _Access}) ->
    {200, [], ""}.
