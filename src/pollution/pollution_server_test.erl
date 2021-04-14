%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. kwi 2021 11:43
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("Ola").

-include_lib("eunit/include/eunit.hrl").


addStation_test() ->
  ?assertMatch(true, pollution_server:start()),

  ?assertMatch(ok, pollution_server:addStation("London Station", {10, 20})),
  ?assertMatch({error, _}, pollution_server:addStation("London Station", {50, 100})),
  ?assertMatch({error, _}, pollution_server:addStation("Berlin Station", {10, 20})),

  ?assertMatch(stop, pollution_server:stop()).


addValue_test() ->
  pollution_server:start(),

  Time = calendar:local_time(),
  pollution_server:addStation("London Station", {10, 20}),

  ?assertMatch(ok, pollution_server:addValue("London Station", Time, "PM10", 120)),
  ?assertMatch({error, _}, pollution_server:addValue({10, 20}, Time, "PM10", 120)),
  ?assertMatch({error, _}, pollution_server:addValue("Berlin Station", calendar:local_time(), "PM20", 20)),

  pollution_server:stop().


removeValue_test() ->
  pollution_server:start(),

  Time = calendar:local_time(),
  pollution_server:addStation("London Station", {10, 20}),
  pollution_server:addValue("London Station", Time, "PM10", 120),

  ?assertMatch(ok, pollution_server:removeValue("London Station", Time, "PM10")),
  ?assertMatch({error, _}, pollution_server:removeValue("London Station", Time, "PM10")),
  ?assertMatch({error, _}, pollution_server:removeValue("Berlin Station", calendar:local_time(), "PM20")),

  pollution_server:stop().


getOneValue_test() ->
  pollution_server:start(),

  Time = calendar:local_time(),
  pollution_server:addStation("London Station", {10, 20}),
  pollution_server:addValue("London Station", Time, "PM10", 120),

  ?assertMatch(120, pollution_server:getOneValue("London Station", Time, "PM10")),
  ?assertMatch({error, _}, pollution_server:getOneValue("London Station", Time, "PM30")),

  pollution_server:stop().


getStationMean_test() ->
  pollution_server:start(),

  pollution_server:addStation("London Station", {10, 20}),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {12, 20, 00}}, "PM10", 10),
  pollution_server:addValue("London Station", {{2021, 03, 20}, {12, 20, 00}}, "PM10", 40),

  ?assertMatch(25.0, pollution_server:getStationMean("London Station", "PM10")),
  ?assertMatch({error, _}, pollution_server:getStationMean("London Station", "temperature")),

  pollution_server:stop().


getDailyMean_test() ->
  pollution_server:start(),

  pollution_server:addStation("London Station", {10, 20}),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {12, 20, 00}}, "PM10", 10),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {18, 20, 00}}, "PM10", 40),

  pollution_server:addStation("Berlin Station", {50, 50}),
  pollution_server:addValue("Berlin Station", {{2021, 03, 28}, {10, 20, 00}}, "PM10", 70),

  ?assertMatch(40.0, pollution_server:getDailyMean("PM10", {{2021, 03, 28}, {00, 00, 00}})),

  pollution_server:stop().


getMinMaxValue_test() ->
  pollution_server:start(),

  pollution_server:addStation("London Station", {10, 20}),

  pollution_server:addValue("London Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 10),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {12, 20, 00}}, "PM10", 5),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {18, 20, 00}}, "PM10", 20),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {16, 20, 00}}, "temperature", 26),

  ?assertMatch({5, 20}, pollution_server:getMinMaxValue({10, 20}, "PM10", {{2021, 03, 28}, {00, 00, 00}})),

  pollution_server:stop().


getNumberOfMeasurements_test() ->
  pollution_server:start(),

  pollution_server:addStation("London Station", {10, 20}),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 10),
  pollution_server:addValue("London Station", {{2021, 03, 29}, {12, 20, 00}}, "temperature", 5),
  pollution_server:addValue("London Station", {{2021, 03, 30}, {18, 20, 00}}, "PM9", 20),
  pollution_server:addValue("London Station", {{2021, 03, 1}, {16, 20, 00}}, "PM10", 30),
  pollution_server:addValue("London Station", {{2021, 03, 2}, {10, 20, 00}}, "temperature", 24),
  pollution_server:addValue("London Station", {{2021, 03, 3}, {20, 20, 00}}, "PM10", 90),

  ?assertMatch(3, pollution_server:getNumberOfMeasurements("London Station", {{2021, 03, 00}, {00, 00, 00}}, "PM10")),
  ?assertMatch(2, pollution_server:getNumberOfMeasurements("London Station", {{2021, 03, 00}, {00, 00, 00}}, "temperature")),
  ?assertMatch(1, pollution_server:getNumberOfMeasurements("London Station", {{2021, 03, 00}, {00, 00, 00}}, "PM9")),

  pollution_server:stop().


getStationWithBiggestNumberOfMeasurements_test() ->
  pollution_server:start(),

  pollution_server:addStation("London Station", {10, 20}),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 10),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {12, 20, 00}}, "temperature", 5),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {18, 20, 00}}, "PM9", 20),
  pollution_server:addValue("London Station", {{2021, 03, 10}, {16, 20, 00}}, "PM10", 30),
  pollution_server:addValue("London Station", {{2021, 03, 10}, {10, 20, 00}}, "temperature", 24),
  pollution_server:addValue("London Station", {{2021, 03, 10}, {20, 20, 00}}, "PM9", 90),

  pollution_server:addStation("Berlin Station", {50, 50}),
  pollution_server:addValue("Berlin Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 5),
  pollution_server:addValue("Berlin Station", {{2021, 03, 29}, {12, 20, 00}}, "PM10", 15),
  pollution_server:addValue("Berlin Station", {{2021, 03, 39}, {18, 20, 00}}, "PM10", 80),

  ?assertMatch("London Station", pollution_server:getStationWithBiggestNumberOfMeasurements("PM9")),
  ?assertMatch("London Station", pollution_server:getStationWithBiggestNumberOfMeasurements("temperature")),
  ?assertMatch("Berlin Station", pollution_server:getStationWithBiggestNumberOfMeasurements("PM10")),

  pollution_server:stop().


getDailyAverageDataCount2_test() ->
  pollution_server:start(),

  pollution_server:addStation("London Station", {10, 20}),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 10),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {12, 20, 00}}, "temperature", 5),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {18, 20, 00}}, "PM9", 20),
  pollution_server:addValue("London Station", {{2021, 03, 10}, {16, 20, 00}}, "PM10", 30),
  pollution_server:addValue("London Station", {{2021, 03, 10}, {10, 20, 00}}, "temperature", 24),
  pollution_server:addValue("London Station", {{2021, 03, 10}, {20, 20, 00}}, "PM9", 90),

  ?assertMatch(3.0, pollution_server:getDailyAverageDataCount("London Station")),

  pollution_server:stop().


getDailyAverageDataCount1_test() ->
  pollution_server:start(),

  pollution_server:addStation("London Station", {10, 20}),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 10),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {12, 20, 00}}, "temperature", 5),
  pollution_server:addValue("London Station", {{2021, 03, 28}, {18, 20, 00}}, "PM9", 20),
  pollution_server:addValue("London Station", {{2021, 03, 10}, {16, 20, 00}}, "PM10", 30),
  pollution_server:addValue("London Station", {{2021, 03, 10}, {10, 20, 00}}, "temperature", 24),
  pollution_server:addValue("London Station", {{2021, 03, 10}, {20, 20, 00}}, "PM9", 90),

  pollution_server:addStation("Berlin Station", {50, 50}),
  pollution_server:addValue("Berlin Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 5),
  pollution_server:addValue("Berlin Station", {{2021, 03, 29}, {12, 20, 00}}, "temperature", 15),
  pollution_server:addValue("Berlin Station", {{2021, 03, 39}, {18, 20, 00}}, "PM9", 80),

  ?assertMatch(2.0, pollution_server:getDailyAverageDataCount()),

  pollution_server:stop().
