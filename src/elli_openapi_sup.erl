%%%-------------------------------------------------------------------
%% @doc elli_openapi top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(elli_openapi_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    Routes =
        [{<<"POST">>, <<"/pelle">>, fun elli_openapi_demo:endpoint/3},
         {<<"GET">>, <<"/user/{userId}/post/{postId}">>, fun elli_openapi_demo:endpoint2/3}],

    ElliOpts = [
            {callback, elli_openapi_handler},
            {callback_args, Routes},
            {port, 3000}
        ],
        ElliSpec = {
            _Id = elli_minimal_http,
            _Start = {elli, start_link, [ElliOpts]},
            _Restart = permanent,
            _Shutdown = 5000,
            _Worker = worker,
            _Modules = [elli]},
        SupFlags =
        #{strategy => one_for_all,
          intensity => 0,
          period => 1},
    ChildSpecs = [ElliSpec],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
