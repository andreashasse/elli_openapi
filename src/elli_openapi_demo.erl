-module(elli_openapi_demo).

-export([endpoint/1]).

-spec endpoint(#{name => string(), shirt_size => small | medium | large}) ->
                  {200, [], #{something => string()}}.
endpoint(#{name := Name, shirt_size := Size}) ->
    {200,
     [],
     #{something => "Hello " ++ Name ++ ", your shirt size is " ++ atom_to_list(Size) ++ "!"}}.
