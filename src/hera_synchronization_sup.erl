%%%-------------------------------------------------------------------
%% @doc hera_synchronization top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hera_synchronization_sup).

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
  MaxRestarts = 6,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => rest_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  {ok, Measurements} = application:get_env(hera_synchronization, measurements),
  SyncMeas = lists:filter(fun({_Name, Sync}) -> Sync end, Measurements),

  Children = [#{id => hera_utils:concat_atoms(hera_global_dispatch_, Name), start => {hera_global_dispatch, start_link, [Name, hera_utils:concat_atoms(dispatch_, Name)]}} || {Name, _Sync} <- SyncMeas],

  GlobalSync = #{id => hera_global_sync,
    start => {hera_global_sync, start_link, []}},

  SupervisorDispatch =#{id => hera_dispatch_sup,
    start => {hera_sup2, start_link, [hera_dispatch_sup, one_for_one, Children]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor},
    {ok, {SupFlags, [GlobalSync, SupervisorDispatch]}}.

%% internal functions
