%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. kwi 2021 11:22
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Ola").

%% API
-export([
  start/0,
  stop/0,
  init/0,
  pollution_loop/1,
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

start() ->
  register(pollutionServer, spawn(fun() -> init() end)).

stop() ->
  pollutionServer ! stop.

init() ->
  Monitor = pollution:createMonitor(),
  pollution_loop(Monitor).

pollution_loop(Monitor) ->
  receive
    stop ->
      io:format("Stopping polution server~n"),
      ok;

    {Pid, addStation, StationName, Coords} ->
      case pollution:addStation(StationName, Coords, Monitor) of
        {error, Msg} ->
          Pid ! {error, Msg},
          pollution_loop(Monitor);
        NewMonitor ->
          Pid ! ok,
          pollution_loop(NewMonitor)
      end;

    {Pid, addValue, Station, Date, Type, Value} ->
      case pollution:addValue(Station, Date, Type, Value, Monitor) of
        {error, Msg} ->
          Pid ! {error, Msg},
          pollution_loop(Monitor);
        NewMonitor ->
          Pid ! ok,
          pollution_loop(NewMonitor)
      end;

    {Pid, removeValue, Station, Date, Type} ->
      case pollution:removeValue(Station, Date, Type, Monitor) of
        {error, Msg} ->
          Pid ! {error, Msg},
          pollution_loop(Monitor);
        NewMonitor ->
          Pid ! ok,
          pollution_loop(NewMonitor)
      end;

    {Pid, getOneValue, Station, Date, Type} ->
      Pid ! pollution:getOneValue(Station, Date, Type, Monitor),
      pollution_loop(Monitor);

    {Pid, getStationMean, Station, Type} ->
      Pid ! pollution:getStationMean(Station, Type, Monitor),
      pollution_loop(Monitor);

    {Pid, getDailyMean, Type, Time} ->
      Pid ! pollution:getDailyMean(Type, Time, Monitor),
      pollution_loop(Monitor);

    {Pid, getMinMaxValue, Coords, Type, Time} ->
      Pid ! pollution:getMinMaxValue(Coords, Type, Time, Monitor),
      pollution_loop(Monitor);

    {Pid, getNumberOfMeasurements, StationName, Time, Type} ->
      Pid ! pollution:getNumberOfMeasurements(StationName, Time, Type, Monitor),
      pollution_loop(Monitor);

    {Pid, getStationWithBiggestNumberOfMeasurements, Type} ->
      Pid ! pollution:getStationWithBiggestNumberOfMeasurements(Type, Monitor),
      pollution_loop(Monitor);

    {Pid, getDailyAverageDataCount, Station} ->
      Pid ! pollution:getDailyAverageDataCount(Station, Monitor),
      pollution_loop(Monitor);

    {Pid, getDailyAverageDataCount} ->
      Pid ! pollution:getDailyAverageDataCount(Monitor),
      pollution_loop(Monitor)

  end.

addStation(StationName, Coords) when is_tuple(Coords) ->
  pollutionServer ! {self(), addStation, StationName, Coords},
  receive
    M -> M
  end.

addValue(Station, Date, Type, Value) ->
  pollutionServer ! {self(), addValue, Station, Date, Type, Value},
  receive
    M -> M
  end.

removeValue(Station, Date, Type) ->
  pollutionServer ! {self(), removeValue, Station, Date, Type},
  receive
    M -> M
  end.

getOneValue(Station, Date, Type) ->
  pollutionServer ! {self(), getOneValue, Station, Date, Type},
  receive
    M -> M
  end.

getStationMean(Station, Type) ->
  pollutionServer ! {self(), getStationMean, Station, Type},
  receive
    M -> M
  end.

getDailyMean(Type, Time) ->
  pollutionServer ! {self(), getDailyMean, Type, Time},
  receive
    M -> M
  end.

getMinMaxValue(Coords, Type, Time) ->
  pollutionServer ! {self(), getMinMaxValue, Coords, Type, Time},
  receive
    M -> M
  end.

getNumberOfMeasurements(StationName, Time, Type) ->
  pollutionServer ! {self(), getNumberOfMeasurements, StationName, Time, Type},
  receive
    M -> M
  end.

getStationWithBiggestNumberOfMeasurements(Type) ->
  pollutionServer ! {self(), getStationWithBiggestNumberOfMeasurements, Type},
  receive
    M -> M
  end.

getDailyAverageDataCount(Station) ->
  pollutionServer ! {self(), getDailyAverageDataCount, Station},
  receive
    M -> M
  end.

getDailyAverageDataCount() ->
  pollutionServer ! {self(), getDailyAverageDataCount},
  receive
    M -> M
  end.