-module(elli_openapi_demo).

-export([create_user/3, get_user/3, echo_text/3, update_status/3, update_item/3]).

-ignore_xref([create_user/3, get_user/3, echo_text/3, update_status/3, update_item/3]).

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
    {200, #{}, Status}.

-record(item, {
    id :: binary(),
    name :: binary(),
    version :: integer()
}).

-record(error_response, {
    message :: binary(),
    code :: binary()
}).

%% Demo endpoint with multiple status codes
-spec update_item(
    #{itemId := binary()},
    #{},
    #{name := binary(), version := integer()}
) ->
    {200, #{'ETag' => binary()}, #item{}}
    | {400, #{}, #error_response{}}
    | {404, #{}, #error_response{}}
    | {409, #{}, #error_response{}}.
update_item(#{itemId := ItemId}, #{}, #{name := Name, version := Version}) ->
    case {ItemId, Name, Version} of
        {~"item-notfound", _, _} ->
            %% Simulate item not found
            {404, #{}, #error_response{
                message = <<"Item not found">>,
                code = <<"ITEM_NOT_FOUND">>
            }};
        {_, _, V} when V < 0 ->
            %% Invalid version number
            {400, #{}, #error_response{
                message = <<"Version must be non-negative">>,
                code = <<"INVALID_VERSION">>
            }};
        {~"item-conflict", _, _} ->
            %% Simulate version conflict
            {409, #{}, #error_response{
                message = <<"Version conflict detected">>,
                code = <<"VERSION_CONFLICT">>
            }};
        {_, _, _} ->
            %% Success case
            Item = #item{
                id = ItemId,
                name = Name,
                version = Version
            },
            ETag = iolist_to_binary(io_lib:format("\"v~p-~s\"", [Version, ItemId])),
            {200, #{'ETag' => ETag}, Item}
    end.
