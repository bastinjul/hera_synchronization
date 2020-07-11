%%%-------------------------------------------------------------------
%% @doc hera_synchronization public API
%% @end
%%%-------------------------------------------------------------------

-module(hera_synchronization).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    hera_synchronization_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
