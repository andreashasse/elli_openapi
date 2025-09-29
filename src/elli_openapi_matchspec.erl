-module(elli_openapi_matchspec).

-export([route_to_matchspec/1, routes_to_matchspecs/1]).

%% @doc Convert a single route to a match specification
-spec route_to_matchspec({atom(), map(), any()}) -> {tuple(), list(), list()}.
route_to_matchspec({_Route, #{path := Path, method := Method}, _HandlerType}) ->
    {PathPattern, Variables} = parse_path(Path),
    % FIXME: Add method to match head.
    MatchHead = {Method, PathPattern},
    Guards = [],
    %% Can not have maps in match spec bodies? I'll have to dig out the things I need.
    Body = {{Path, Variables}},
    {MatchHead, Guards, [Body]}.

%% @doc Convert multiple routes to match specifications
-spec routes_to_matchspecs([{atom(), map(), any()}]) -> [{tuple(), list(), list()}].
routes_to_matchspecs(Routes) ->
    lists:map(fun route_to_matchspec/1, Routes).

%% @doc Parse a path string and extract variables, returning a pattern and variable list
-spec parse_path(binary()) -> {tuple(), [{atom(), atom()}]}.
parse_path(Path) ->
    PathTokens = binary:split(Path, <<"/">>, [global, trim_all]),
    {Pattern, Variables, _VarNum} =
        lists:foldl(fun parse_path_token/2, {[], [], 1}, PathTokens),
    {list_to_tuple(lists:reverse(Pattern)), lists:reverse(Variables)}.

%% @doc Parse a single path token, handling variables in {VarName} format
-spec parse_path_token(binary(), {list(), list(), integer()}) ->
                          {list(), list(), integer()}.
parse_path_token(Token, {Pattern, Variables, VarNum}) ->
    case is_variable(Token) of
        {true, VarName} ->
            VarAtom = list_to_atom("$" ++ integer_to_list(VarNum)),
            NewPattern = [VarAtom | Pattern],
            NewVariables = [{{binary_to_atom(VarName), VarAtom}} | Variables],
            {NewPattern, NewVariables, VarNum + 1};
        false ->
            {[Token | Pattern], Variables, VarNum}
    end.

%% @doc Check if a token is a variable (wrapped in curly braces)
-spec is_variable(binary()) -> {true, binary()} | false.
is_variable(Binary) ->
    case re:run(Binary, <<"^\\{(.*)\\}$">>, [{capture, all_but_first, binary}]) of
        {match, [VarName]} ->
            {true, VarName};
        nomatch ->
            false
    end.
