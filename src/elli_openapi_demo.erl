-module(elli_openapi_demo).

-export([create_user/4, get_user/4, echo_text/4, update_status/4, update_item/4, list_users/4]).

-ignore_xref([create_user/4, get_user/4, echo_text/4, update_status/4, update_item/4, list_users/4]).

-compile(nowarn_unused_type).

-record(user, {
    id :: binary(),
    email :: binary(),
    name :: binary(),
    role :: admin | user | guest
}).

-record(item, {
    id :: binary(),
    name :: binary(),
    version :: integer()
}).

-record(error_response, {
    message :: binary(),
    code :: binary()
}).

-record(user_list, {
    users :: [#user{}],
    total :: non_neg_integer()
}).

-spectra(#{summary => <<"Create a new user">>, description => <<"Creates a user account">>}).
-spec create_user(
    #{},
    #{},
    #{},
    #{email := binary(), name := binary(), role => admin | user | guest}
) ->
    {201, #{'Location' => binary(), 'ETag' => binary()}, #user{}}.

-spectra(#{summary => <<"Get a user by ID">>}).
-spec get_user(
    #{userId := binary()},
    #{},
    #{'Authorization' := binary()},
    binary()
) ->
    {200, #{'ETag' => binary(), 'Cache-Control' => binary()}, #user{}}.

-spec echo_text(#{}, #{}, #{}, binary()) -> {200, #{}, binary()}.

-spec update_status(#{}, #{}, #{}, running | stopped | paused) ->
    {200, #{}, running | stopped | paused}.

-spectra(#{
    summary => <<"Update an item">>,
    description => <<"Updates item by ID, with conflict detection">>
}).
-spec update_item(
    #{itemId := binary()},
    #{},
    #{},
    #{name := binary(), version := integer()}
) ->
    {200, #{'ETag' => binary()}, #item{}}
    | {400, #{}, #error_response{}}
    | {404, #{}, #error_response{}}
    | {409, #{}, #error_response{}}.

-spectra(#{summary => <<"List users">>, description => <<"Returns a paginated list of users">>}).
-spec list_users(
    #{},
    #{page => pos_integer(), per_page => pos_integer()},
    #{},
    binary()
) ->
    {200, #{}, #user_list{}}.

%%====================================================================
%% Function definitions
%%====================================================================

create_user(#{}, #{}, #{}, #{email := Email, name := Name} = Body) ->
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

get_user(#{userId := UserId}, #{}, #{'Authorization' := _Token}, ~"") ->
    User = #user{
        id = UserId,
        email = <<"user@example.com">>,
        name = <<"John Doe">>,
        role = user
    },
    ETag = <<"\"v1-", UserId/binary, "\"">>,
    CacheControl = <<"max-age=300, must-revalidate">>,
    {200, #{'ETag' => ETag, 'Cache-Control' => CacheControl}, User}.

echo_text(#{}, #{}, #{}, Text) ->
    {200, #{}, <<"Echo: ", Text/binary>>}.

update_status(#{}, #{}, #{}, Status) ->
    {200, #{}, Status}.

update_item(#{itemId := ItemId}, #{}, #{}, #{name := Name, version := Version}) ->
    case {ItemId, Name, Version} of
        {~"item-notfound", _, _} ->
            {404, #{}, #error_response{
                message = <<"Item not found">>,
                code = <<"ITEM_NOT_FOUND">>
            }};
        {_, _, V} when V < 0 ->
            {400, #{}, #error_response{
                message = <<"Version must be non-negative">>,
                code = <<"INVALID_VERSION">>
            }};
        {~"item-conflict", _, _} ->
            {409, #{}, #error_response{
                message = <<"Version conflict detected">>,
                code = <<"VERSION_CONFLICT">>
            }};
        {_, _, _} ->
            Item = #item{
                id = ItemId,
                name = Name,
                version = Version
            },
            ETag = iolist_to_binary(io_lib:format("\"v~p-~s\"", [Version, ItemId])),
            {200, #{'ETag' => ETag}, Item}
    end.

list_users(#{}, QueryParams, #{}, ~"") ->
    _Page = maps:get(page, QueryParams, 1),
    _PerPage = maps:get(per_page, QueryParams, 20),
    Users = [
        #user{id = <<"user-1">>, email = <<"a@example.com">>, name = <<"Alice">>, role = admin},
        #user{id = <<"user-2">>, email = <<"b@example.com">>, name = <<"Bob">>, role = user}
    ],
    {200, #{}, #user_list{users = Users, total = 2}}.
