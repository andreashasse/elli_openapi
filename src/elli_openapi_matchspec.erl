-module(elli_openapi_matchspec).

-export([route_to_matchspec/1, routes_to_matchspecs/1]).

%% @doc Convert a single route to a match specification
-spec route_to_matchspec({atom(), string(), fun()}) -> {tuple(), list(), list()}.
route_to_matchspec({Method, Path, Fun}) ->
    {PathPattern, Variables} = parse_path(Path),
    MatchHead = PathPattern,
    Guards = [],
    Body = {{Method, Fun, Variables}},
    {MatchHead, Guards, [Body]}.

%% @doc Convert multiple routes to match specifications
-spec routes_to_matchspecs([{atom(), string(), fun()}]) -> [{tuple(), list(), list()}].
routes_to_matchspecs(Routes) ->
    lists:map(fun route_to_matchspec/1, Routes).

%% @doc Parse a path string and extract variables, returning a pattern and variable list
-spec parse_path(string()) -> {tuple(), [{atom(), atom()}]}.
parse_path(Path) ->
    PathTokens = string:tokens(Path, "/"),
    {Pattern, Variables, _VarNum} =
        lists:foldl(fun parse_path_token/2, {[], [], 1}, PathTokens),
    {list_to_tuple(lists:reverse(Pattern)), lists:reverse(Variables)}.

%% @doc Parse a single path token, handling variables in {VarName} format
-spec parse_path_token(string(), {list(), list(), integer()}) ->
                          {list(), list(), integer()}.
parse_path_token(Token, {Pattern, Variables, VarNum}) ->
    case is_variable(Token) of
        {true, VarName} ->
            VarAtom = list_to_atom("$" ++ integer_to_list(VarNum)),
            NewPattern = [VarAtom | Pattern],
            NewVariables = [{{list_to_atom(VarName), VarAtom}} | Variables],
            {NewPattern, NewVariables, VarNum + 1};
        false ->
            {[Token | Pattern], Variables, VarNum}
    end.

%% @doc Check if a token is a variable (wrapped in curly braces)
-spec is_variable(string()) -> {true, string()} | false.
is_variable([${ | Rest]) ->
    case lists:reverse(Rest) of
        [$} | ReversedVarName] ->
            {true, lists:reverse(ReversedVarName)};
        _ ->
            false
    end;
is_variable(_) ->
    false.
