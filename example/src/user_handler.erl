-module(user_handler).

-export([get_user/4, create_user/4]).

-record(user, {
    id :: binary(),
    name :: binary(),
    role :: admin | user | guest
}).

-ignore_xref([create_user/4, get_user/4]).

-spec get_user(#{userId := binary()}, #{}, #{}, binary()) ->
    {200, #{}, #user{}}
    | {404, #{}, #{message := binary()}}.
get_user(#{userId := Id}, #{}, _Hdrs, _Body) ->
    case find_user(Id) of
        {ok, User} -> {200, #{}, User};
        not_found -> {404, #{}, #{message => ~"User not found"}}
    end.

-spec create_user(#{}, #{}, #{}, #user{}) ->
    {201, #{'Location' => binary()}, #user{}}.
create_user(#{}, #{}, #{}, User) ->
    io:format("Creating user: ~s with role ~p~n", [User#user.name, User#user.role]),
    Location = <<"/api/users/", (User#user.id)/binary>>,
    {201, #{'Location' => Location}, User}.

find_user(~"123") ->
    {ok, #user{id = ~"123", name = ~"Alice", role = user}};
find_user(_) ->
    not_found.
