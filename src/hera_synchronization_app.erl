%%%-------------------------------------------------------------------
%% @doc hera_synchronization public API
%% @end
%%%-------------------------------------------------------------------

-module(hera_synchronization_app).

-behaviour(application).

-export([start/2, stop/1]).

start(StartType, _StartArgs) when StartType =/= normal ->
    logger:notice("hera_synchronization restarted on other node with type : ~p~n", [StartType]),
    {ok, Measurements} = application:get_env(hera_synchronization, measurements),
    [hera:maybe_propagate(fun() -> hera:restart_measurement(Meas, true) end) || {Meas, _} <- Measurements],
    hera_synchronization_sup:start_link();
start(_StartType, _StartArgs) ->
    hera_synchronization_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
