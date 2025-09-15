-module(elli_openapi_demo).

-export([endpoint/3, endpoint2/3]).

-compile(nowarn_unused_type).

-record(user,
        {first_name :: string(),
         last_name :: string(),
         age :: integer() | undefined,
         access :: [read | write | delete]}).

-spec endpoint(#{}, #{}, #{name => string(), shirt_size => small | medium | large}) ->
                  {200, [], #{something => string()}}.
endpoint(#{}, #{}, #{name := Name, shirt_size := Size}) ->
    {200,
     [],
     #{something => "Hello " ++ Name ++ ", your shirt size is " ++ atom_to_list(Size) ++ "!"}}.

-spec endpoint2(#{userId := string(), postId := string()}, #{'User-Agent' := string()}, #user{}) -> {200, [], string()}.
endpoint2(#{userId := UserId, postId := PostId}, #{'User-Agent' := UserAgent}, #user{access = _Access} = User) ->
    io:format("User ~s with Agent ~p requested post ~w~n~p~n", [UserId, UserAgent, PostId, User]),
    {200, [], ""}.
