-module(elli_openapi_demo).

-export([create_user/3, get_user/3, echo_text/3, update_status/3]).

-ignore_xref([create_user/3, get_user/3, echo_text/3, update_status/3]).

-compile(nowarn_unused_type).

-record(user, {
    id :: binary(),
    email :: binary(),
    name :: binary(),
    role :: admin | user | guest
}).

-spec create_user(
    #{},
    #{},
    #{email := binary(), name := binary(), role => admin | user | guest}
) ->
    {201, #{'Location' => binary(), 'ETag' => binary()}, #user{}}.
create_user(#{}, #{}, #{email := Email, name := Name} = Body) ->
    Role = maps:get(role, Body, user),
    UserId = <<"user-123">>,
    User = #user{
        id = UserId,
        email = Email,
        name = Name,
        role = Role
    },
    Location = <<"/api/users/", UserId/binary>>,
    ETag = <<"\"v1-", UserId/binary, "\"">>,
    {201, #{'Location' => Location, 'ETag' => ETag}, User}.

-spec get_user(
    #{userId := binary()},
    #{'Authorization' := binary()},
    binary()
) ->
    {200, #{'ETag' => binary(), 'Cache-Control' => binary()}, #user{}}.
get_user(#{userId := UserId}, #{'Authorization' := _Token}, ~"") ->
    User = #user{
        id = UserId,
        email = <<"user@example.com">>,
        name = <<"John Doe">>,
        role = user
    },
    ETag = <<"\"v1-", UserId/binary, "\"">>,
    CacheControl = <<"max-age=300, must-revalidate">>,
    {200, #{'ETag' => ETag, 'Cache-Control' => CacheControl}, User}.

-spec echo_text(#{}, #{}, binary()) -> {200, #{}, binary()}.
echo_text(#{}, #{}, Text) ->
    {200, #{}, <<"Echo: ", Text/binary>>}.

-spec update_status(#{}, #{}, running | stopped | paused) ->
    {200, #{}, running | stopped | paused}.
update_status(#{}, #{}, Status) ->
    %% Echo the status back
    {200, #{}, Status}.
