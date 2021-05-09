%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. kwi 2021 12:23
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("Ola").

-behaviour(gen_statem).

%% API
-export([init/1, start_link/0, callback_mode/0, terminate/3, store/4, stop/0]).
-export([setStation/1, addValue/3, storeData/0]).
-export([idle/3, collect_values/3]).


%% START %%
start_link() -> gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).
callback_mode() -> state_functions.
init([]) -> {ok, idle, []}.

setStation(Station) -> gen_statem:cast(?MODULE, {set_station, Station}).
addValue(Date, Type, Value) -> gen_statem:cast(?MODULE, {add_value, Date, Type, Value}).
storeData() -> gen_statem:cast(?MODULE, flush_data).

%% HANDLERS %%
idle(_Event, {set_station, Station}, _None) ->
  {next_state, collect_values, #{station => Station, values => []}}.
collect_values(_Event, {add_value, Date, Type, Value}, #{values := Values} = Data) ->
  {next_state, collect_values, Data#{values => [{Date, Type, Value} | Values]}};
collect_values(_Event, flush_data, #{station := Station, values := Values}) ->
  %% store data
  [store(Station, Date, Type, Value) || {Date, Type, Value} <- Values],
  {next_state, idle, []}.

terminate(Reason, Station, StateData) ->
  io:format("STATEM: terminating due to ~w, in state ~w with data ~w ~n", [Reason, Station, StateData]).

store(Station, Date, Type, Value) ->
  io:format("Add Value: ~w ~w ~w ~w ~n", [Station, Date, Type, Value]),
  pollution_gen_server:addValue(Station, Date, Type, Value).

stop() -> gen_statem:stop(?MODULE).