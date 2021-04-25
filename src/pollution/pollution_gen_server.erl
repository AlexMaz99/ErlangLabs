%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. kwi 2021 11:33
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("Ola").
-behaviour(gen_server).

%% API
-export([init/1, handle_cast/2, handle_info/2, handle_call/3, terminate/2]).
-export([
  start_link/0,
  crash/0,
  addStation/2,
  addValue/4,
  removeValue/3,
  getOneValue/3,
  getStationMean/2,
  getDailyMean/2,
  getMinMaxValue/3,
  getNumberOfMeasurements/3,
  getStationWithBiggestNumberOfMeasurements/1,
  getDailyAverageDataCount/1,
  getDailyAverageDataCount/0
]).


%% START %%
start_link() -> gen_server:start_link({local,?MODULE}, ?MODULE, pollution:createMonitor(), []).
init(N) -> {ok,N}.


%% INTERFACE CLIENT -> SERVER %%
crash() -> gen_server:cast(?MODULE, crash).
addStation(StationName, Coords) -> gen_server:call(?MODULE, {addStation, StationName, Coords}).
addValue(Station, Date, Type, Value) -> gen_server:call(?MODULE, {addValue, Station, Date, Type, Value}).
removeValue(Station, Date, Type) -> gen_server:call(?MODULE, {removeValue, Station, Date, Type}).
getOneValue(Station, Date, Type) -> gen_server:call(?MODULE, {getOneValue, Station, Date, Type}).
getStationMean(Station, Type) -> gen_server:call(?MODULE, {getStationMean, Station, Type}).
getDailyMean(Type, Time) -> gen_server:call(?MODULE, {getDailyMean, Type, Time}).
getMinMaxValue(Coords, Type, Time) -> gen_server:call(?MODULE, {getMinMaxValue, Coords, Type, Time}).
getNumberOfMeasurements(StationName, Time, Type) -> gen_server:call(?MODULE, {getNumberOfMeasurements, StationName, Time, Type}).
getStationWithBiggestNumberOfMeasurements(Type) -> gen_server:call(?MODULE, {getStationWithBiggestNumberOfMeasurements, Type}).
getDailyAverageDataCount(Station) -> gen_server:call(?MODULE, {getDailyAverageDataCount, Station}).
getDailyAverageDataCount() -> gen_server:call(?MODULE, {getDailyAverageDataCount}).


%% HANDLE MESSAGES %%
handle_call({addStation, StationName, Coords}, _From, Monitor) ->
  case pollution:addStation(StationName, Coords, Monitor) of
    {error, Msg} -> {reply, Msg, Monitor};
    NewMonitor -> {reply, ok, NewMonitor}
  end;

handle_call({addValue, Station, Date, Type, Value}, _From, Monitor) ->
  case pollution:addValue(Station, Date, Type, Value, Monitor) of
    {error, Msg} -> {reply, Msg, Monitor};
    NewMonitor -> {reply, ok, NewMonitor}
  end;

handle_call({removeValue, Station, Date, Type}, _From, Monitor) ->
  case pollution:removeValue(Station, Date, Type, Monitor) of
    {error, Msg} -> {reply, Msg, Monitor};
    NewMonitor -> {reply, ok, NewMonitor}
  end;

handle_call({getOneValue, Station, Date, Type}, _From, Monitor) ->
  {reply, pollution:getOneValue(Station, Date, Type, Monitor), Monitor};

handle_call({getStationMean, Station, Type}, _From, Monitor) ->
  {reply, pollution:getStationMean(Station, Type, Monitor), Monitor};

handle_call({getDailyMean, Type, Time}, _From, Monitor) ->
  {reply, pollution:getDailyMean(Type, Time, Monitor), Monitor};

handle_call({getMinMaxValue, Coords, Type, Time}, _From, Monitor) ->
  {reply, pollution:getMinMaxValue(Coords, Type, Time, Monitor), Monitor};

handle_call({getNumberOfMeasurements, StationName, Time, Type}, _From, Monitor) ->
  {reply, pollution:getNumberOfMeasurements(StationName, Time, Type, Monitor), Monitor};

handle_call({getStationWithBiggestNumberOfMeasurements, Type}, _From, Monitor) ->
  {reply, pollution:getStationWithBiggestNumberOfMeasurements(Type, Monitor), Monitor};

handle_call({getDailyAverageDataCount, Station}, _From, Monitor) ->
  {reply, pollution:getDailyAverageDataCount(Station, Monitor), Monitor};

handle_call({getDailyAverageDataCount}, _From, Monitor) ->
  {reply, pollution:getDailyAverageDataCount(Monitor), Monitor}.


handle_info(_, N) -> {noreply, N}.

handle_cast(crash, M) ->
  pollution:noExistingFunction(),
  {noreply, M}.

terminate(normal, _) -> io:format("Normal terminate~n"), ok;
terminate(other, _) -> io:format("Other terminate~n"), ok;
terminate(kill, _) -> io:format("Kill terminate~n"), ok.