%%%-------------------------------------------------------------------
%%% @author Aleksandra Mazur
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. mar 2021 15:04
%%%-------------------------------------------------------------------
-module(pollution).
-author("Aleksandra Mazur").

%% API
-export([
  createMonitor/0,
  addStation/3,
  stationExists/2,
  addValue/5,
  getStation/2,
  removeValue/4,
  getOneValue/4,
  getStationMean/3,
  getDailyMean/3,
  getMinMax/2,
  getMinMaxValue/4,
  getDailyAverageDataCount/2,
  getDailyAverageDataCount/1,
  getNumberOfMeasurements/4,
  getStationWithBiggestNumberOfMeasurements/2
]).

%% 'monitor' - record which contains two dictionaries:
%%  - 'stations' - key: station's name, value: record 'station'
%%  - 'coords' - key: station's coordinates, value: station's name

%% 'station' - record which contains:
%%  - 'name' - station's name
%%  - 'coords' - station's coordinates {X, Y}
%%  - 'measurements' - dictionary with key: {datetime, type of measurement}, value: record 'measurement'

%% 'measurement' - record which contains:
%%  - 'type' - type of measurement
%%  - 'datetime' - date and time of measurement
%%  - 'value' - value of measurement

-record(measurement, {type, datetime, value}).
-record(station, {name, coords, measurements}).
-record(monitor, {stations, coords}).


%% ------------------------- createMonitor -------------------------
%% creates and returns new monitor
createMonitor() ->
  #monitor{stations = dict:new(), coords = dict:new()}.


%% ------------------------- stationExists -------------------------
%% checks if given station exists
stationExists(Coords, Monitor) when is_tuple(Coords) ->
  dict:is_key(Coords, Monitor#monitor.coords);

stationExists(StationName, Monitor) ->
  dict:is_key(StationName, Monitor#monitor.stations).


%% -------------------------- addStation --------------------------
%% adds a new station to monitor and returns new monitor
addStation(StationName, Coords, Monitor) when is_tuple(Coords) ->
  case stationExists(StationName, Monitor) orelse stationExists(Coords, Monitor) of
    true -> {error, "Given station already exists"};
    false ->
      NewStations = dict:append(StationName, #station{name = StationName, coords = Coords, measurements = dict:new()}, Monitor#monitor.stations),
      NewCoords = dict:append(Coords, StationName, Monitor#monitor.coords),
      Monitor#monitor{stations = NewStations, coords = NewCoords}
  end.


%% -------------------------- getStation --------------------------
%% returns the station with the given name or coordinates
getStation(Coords, Monitor) when is_tuple(Coords) ->
  case dict:find(Coords, Monitor#monitor.coords) of
    error -> {error, "Given station does not exist"};
    {ok, [Name | _]} -> getStation(Name, Monitor)
  end;

getStation(StationName, Monitor) ->
  case dict:find(StationName, Monitor#monitor.stations) of
    error -> {error, "Given station does not exist"};
    {ok, [Station | _]} -> Station
  end.


%% --------------------------- addValue ---------------------------
%% adds a measurement from the station and updates the monitor
addValue(Station, Date, Type, Value, Monitor) ->
  case stationExists(Station, Monitor) of
    false -> {error, "Given station does not exist"};
    true ->
      CurrentStation = getStation(Station, Monitor),

      case dict:is_key({Date, Type}, CurrentStation#station.measurements) of
        true -> {error, "Measuerement already exists"};
        false ->
          Measurement = #measurement{type = Type, datetime = Date, value = Value},
          #monitor{
            stations = dict:update(
              CurrentStation#station.name,
              fun([Old | _]) ->
                [Old#station{measurements = dict:append({Date, Type}, Measurement, Old#station.measurements)}]
              end,
              Monitor#monitor.stations
            ),
            coords = Monitor#monitor.coords
          }
      end
  end.


%% -------------------------- removeValue --------------------------
%% removes a measurement from the station and updates the monitor
removeValue(Station, Date, Type, Monitor) ->
  case stationExists(Station, Monitor) of
    false -> {error, "Given station does not exist"};
    true ->
      OldStation = getStation(Station, Monitor),

      case dict:is_key({Date, Type}, OldStation#station.measurements) of
        false -> {error, "Measuerement does not exist"};
        true ->
          #monitor{
            stations = dict:update(
              OldStation#station.name,
              fun([Old | _]) ->
                [Old#station{measurements = dict:erase({Date, Type}, Old#station.measurements)}]
              end,
              Monitor#monitor.stations
            ),
            coords = Monitor#monitor.coords
          }
      end
  end.


%% ------------------------- getOneValue -------------------------
%% returns the value of the measurement from a given station with a given type and date
getOneValue(Station, Date, Type, Monitor) ->
  case stationExists(Station, Monitor) of
    false -> {error, "Station doest not exist"};
    true ->
      CurrentStation = getStation(Station, Monitor),
      case dict:find({Date, Type}, CurrentStation#station.measurements) of
        error -> {error, "Measurement does not exist"};
        {ok, [Measurement | _]} ->
          Measurement#measurement.value
      end
  end.


%% ------------------------ getStationMean ------------------------
%% returns the average value of a parameter of a given type from a given station
getStationMean(Station, Type, Monitor) ->
  case stationExists(Station, Monitor) of
    false -> {error, "Given station does not exist"};
    true ->
      CurrentStation = getStation(Station, Monitor),
      Measurements = dict:filter(
        fun({D, T}, Value) -> T == Type end,
        CurrentStation#station.measurements
      ),

      case dict:size(Measurements) of
        0 -> {error, "No measurements with given type"};
        _ ->
          ValuesSum = dict:fold(
            fun(Key, [Measurement | _], AccIn) -> AccIn + Measurement#measurement.value end,
            0,
            Measurements
          ),
          ValuesSum / dict:size(Measurements)
      end
  end.


%% ------------------------- getDailyMean -------------------------
%% returns the average value of a parameter of a given type on a given day at all stations
getDailyMean(Type, {{Year, Month, Day}, _}, Monitor) ->
  {Sum, Size} = dict:fold(
    fun(StationName, [Station | _], {ValuesSum, ValuesSize}) ->

      Measurements = dict:filter(
        fun({{{Y, M, D}, _}, T}, Value) ->
          (Year == Y) and (Month == M) and (Day == D) and (Type == T)
        end,
        Station#station.measurements
      ),

      MeasurementsSum = dict:fold(
        fun(Key, [Measurement | _], AccIn) -> AccIn + Measurement#measurement.value end,
        0,
        Measurements
      ),

      {ValuesSum + MeasurementsSum, ValuesSize + dict:size(Measurements)}
    end,
    {0, 0},
    Monitor#monitor.stations
  ),

  case Size of
    0 -> {error, "No measurements with given parameters"};
    _ -> Sum / Size
  end.


%% ---------------------------- getMinMax ----------------------------
%% returns min and max value from three given numbers
getMinMax(X, {Min, Max}) when X < Min ->
  {X, Max};

getMinMax(X, {Min, Max}) when X > Max ->
  {Min, X};

getMinMax(_, V) -> V.


%% -------------------------- getMinMaxValue --------------------------
%% returns the minimum and maximum value of a measurement of a given type on the given day and station's coordinates
getMinMaxValue(Coords, Type, {{Year, Month, Day}, _}, Monitor) ->
  case stationExists(Coords, Monitor) of
    false -> {error, "Given station does not exist"};
    true ->
      Station = getStation(Coords, Monitor),

      Measurements = dict:filter(
        fun({{{Y, M, D}, _}, T}, Value) ->
          (Year == Y) and (Month == M) and (Day == D) and (Type == T)
        end,
        Station#station.measurements
      ),

      dict:fold(
        fun(Key, [Measurement | _], {Min, Max}) ->
          getMinMax(Measurement#measurement.value, {Min, Max})
        end,
        {134217728, -134217729},
        Measurements
      )
  end.


%% ------------------------ getNumberOfMeasurements ------------------------
%% returns the number of measurements of a given type in a given month of the year at a given station
getNumberOfMeasurements(StationName, {{Year, Month, _}, _}, Type, Monitor) ->
  case stationExists(StationName, Monitor) of
    false -> {error, "Given station does not exist"};
    true ->
      Station = getStation(StationName, Monitor),
      Measurements = dict:filter(
        fun({{{Y, M, D}, _}, T}, Value) ->
          (Year == Y) and (Month == M) and (Type == T)
        end,
        Station#station.measurements
      ),
      dict:size(Measurements)
  end.


%% ---------------- getStationWithBiggestNumberOfMeasurements ----------------
%% returns the name of the station with the greatest number of measurements of the given type
getStationWithBiggestNumberOfMeasurements(Type, Monitor) ->
  {Name, _} = dict:fold(
    fun(Name, [Station | _], {StationName, MaxValue}) ->

      Measurements = dict:filter(
        fun({D, T}, V) -> (Type == T) end,
        Station#station.measurements
      ),

      case dict:size(Measurements) > MaxValue of
        true -> {Name, dict:size(Measurements)};
        false -> {StationName, MaxValue}
      end
    end,
    {"", 0},
    Monitor#monitor.stations
  ),

  Name.


%% --------------------- getDailyAverageDataCount ---------------------
%% returns daily average number of measurements on given station
getDailyAverageDataCount(Station, Monitor) ->
  case stationExists(Station, Monitor) of
    false -> {error, "Given station does not exist"};
    true ->
      CurrentStation = getStation(Station, Monitor),

      Days = dict:fold(
        fun({{{Y, M, D}, _}, T}, [Measurement | _], AccIn) ->
          dict:append({Y, M, D}, Measurement#measurement.value, AccIn)
        end,
        dict:new(),
        CurrentStation#station.measurements
      ),

      NumberOfDays = dict:size(Days),
      NumberOfMeasurements = dict:size(CurrentStation#station.measurements),
      NumberOfMeasurements / NumberOfDays
  end.

%% returns daily average number of measurements of all stations
getDailyAverageDataCount(Monitor) ->
  Names = dict:fetch_keys(Monitor#monitor.stations),

  AverageSum = lists:foldl(
    fun(X, Acc) -> getDailyAverageDataCount(X, Monitor) + Acc end,
    0,
    Names
  ),

  NumberOfStations = lists:foldl(
    fun(_, Acc) -> 1 + Acc end,
    0,
    Names
  ),

  case NumberOfStations of
    0 -> {error, "No stations"};
    _ -> AverageSum / NumberOfStations
  end.