%%%-------------------------------------------------------------------
%%% @author Aleksandra Mazur
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. mar 2021 12:41
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("Aleksandra Mazur").

-include_lib("eunit/include/eunit.hrl").


addStation_test() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("London Station", {10, 20}, M1),

  ?assertMatch({error, _}, pollution:addStation("London Station", {50, 100}, M2)),
  ?assertMatch({error, _}, pollution:addStation("Berlin Station", {10, 20}, M2)).


stationExists_test() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("London Station", {10, 20}, M1),

  ?assert(pollution:stationExists("London Station", M2)),
  ?assert(pollution:stationExists({10, 20}, M2)),
  ?assert(not pollution:stationExists("Berlin Station", M2)).


addValue_test() ->
  Time = calendar:local_time(),
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("London Station", {10, 20}, M1),
  M3 = pollution:addValue({10, 20}, Time, "PM10", 120, M2),

  ?assertMatch({error, _}, pollution:addValue("London Station", Time, "PM10", 10, M3)),
  ?assertMatch({error, _}, pollution:addValue("Berlin Station", calendar:local_time(), "PM20", 20, M3)).


removeValue_test() ->
  Time = calendar:local_time(),
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("London Station", {10, 20}, M1),
  M3 = pollution:addValue({10, 20}, Time, "PM10", 120, M2),
  M4 = pollution:removeValue({10, 20}, Time, "PM10", M3),

  ?assertMatch({error, _}, pollution:removeValue({10, 20}, Time, "PM10", M4)),
  ?assertMatch({error, _}, pollution:removeValue("London Station", calendar:local_time(), "PM20", M4)).


getOneValue_test() ->
  Time = calendar:local_time(),
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("London Station", {10, 20}, M1),
  M3 = pollution:addValue({10, 20}, Time, "PM10", 120, M2),

  ?assertMatch(120, pollution:getOneValue("London Station", Time, "PM10", M3)),
  ?assertMatch({error, _}, pollution:getOneValue("London Station", Time, "PM30", M3)).


getStationMean_test() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("London Station", {10, 20}, M1),
  M3 = pollution:addValue({10, 20}, {{2021, 03, 28}, {12, 20, 00}}, "PM10", 10, M2),
  M4 = pollution:addValue("London Station", {{2021, 03, 20}, {12, 20, 00}}, "PM10", 40, M3),

  ?assertMatch(25.0, pollution:getStationMean("London Station", "PM10", M4)),
  ?assertMatch({error, _}, pollution:getStationMean("London Station", "temperature", M4)).


getDailyMean_test() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("London Station", {10, 20}, M1),
  M3 = pollution:addValue("London Station", {{2021, 03, 28}, {12, 20, 00}}, "PM10", 10, M2),
  M4 = pollution:addValue("London Station", {{2021, 03, 28}, {18, 20, 00}}, "PM10", 40, M3),

  M5 = pollution:addStation("Berlin Station", {50, 50}, M4),
  M6 = pollution:addValue("Berlin Station", {{2021, 03, 28}, {10, 20, 00}}, "PM10", 70, M5),

  ?assertMatch(40.0, pollution:getDailyMean("PM10", {{2021, 03, 28}, {00, 00, 00}}, M6)).


getMinMaxValue_test() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("London Station", {10, 20}, M1),
  M3 = pollution:addValue("London Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 10, M2),
  M4 = pollution:addValue("London Station", {{2021, 03, 28}, {12, 20, 00}}, "PM10", 5, M3),
  M5 = pollution:addValue("London Station", {{2021, 03, 28}, {18, 20, 00}}, "PM10", 20, M4),
  M6 = pollution:addValue("London Station", {{2021, 03, 28}, {16, 20, 00}}, "temperature", 26, M5),

  ?assertMatch({5, 20}, pollution:getMinMaxValue({10, 20}, "PM10", {{2021, 03, 28}, {00, 00, 00}}, M6)).


getNumberOfMeasurements_test() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("London Station", {10, 20}, M1),
  M3 = pollution:addValue("London Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 10, M2),
  M4 = pollution:addValue("London Station", {{2021, 03, 29}, {12, 20, 00}}, "temperature", 5, M3),
  M5 = pollution:addValue("London Station", {{2021, 03, 30}, {18, 20, 00}}, "PM9", 20, M4),
  M6 = pollution:addValue("London Station", {{2021, 03, 1}, {16, 20, 00}}, "PM10", 30, M5),
  M7 = pollution:addValue("London Station", {{2021, 03, 2}, {10, 20, 00}}, "temperature", 24, M6),
  M8 = pollution:addValue("London Station", {{2021, 03, 3}, {20, 20, 00}}, "PM10", 90, M7),

  ?assertMatch(3, pollution:getNumberOfMeasurements("London Station", {{2021, 03, 00}, {00, 00, 00}}, "PM10", M8)),
  ?assertMatch(2, pollution:getNumberOfMeasurements("London Station", {{2021, 03, 00}, {00, 00, 00}}, "temperature", M8)),
  ?assertMatch(1, pollution:getNumberOfMeasurements("London Station", {{2021, 03, 00}, {00, 00, 00}}, "PM9", M8)).


getStationWithBiggestNumberOfMeasurements_test() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("London Station", {10, 20}, M1),
  M3 = pollution:addValue("London Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 10, M2),
  M4 = pollution:addValue("London Station", {{2021, 03, 28}, {12, 20, 00}}, "temperature", 5, M3),
  M5 = pollution:addValue("London Station", {{2021, 03, 28}, {18, 20, 00}}, "PM9", 20, M4),
  M6 = pollution:addValue("London Station", {{2021, 03, 10}, {16, 20, 00}}, "PM10", 30, M5),
  M7 = pollution:addValue("London Station", {{2021, 03, 10}, {10, 20, 00}}, "temperature", 24, M6),
  M8 = pollution:addValue("London Station", {{2021, 03, 10}, {20, 20, 00}}, "PM9", 90, M7),

  M9 = pollution:addStation("Berlin Station", {50, 50}, M8),
  M10 = pollution:addValue("Berlin Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 5, M9),
  M11 = pollution:addValue("Berlin Station", {{2021, 03, 29}, {12, 20, 00}}, "PM10", 15, M10),
  M12 = pollution:addValue("Berlin Station", {{2021, 03, 39}, {18, 20, 00}}, "PM10", 80, M11),

  ?assertMatch("London Station", pollution:getStationWithBiggestNumberOfMeasurements("PM9", M12)),
  ?assertMatch("London Station", pollution:getStationWithBiggestNumberOfMeasurements("temperature", M12)),
  ?assertMatch("Berlin Station", pollution:getStationWithBiggestNumberOfMeasurements("PM10", M12)).


getDailyAverageDataCount2_test() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("London Station", {10, 20}, M1),
  M3 = pollution:addValue("London Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 10, M2),
  M4 = pollution:addValue("London Station", {{2021, 03, 28}, {12, 20, 00}}, "temperature", 5, M3),
  M5 = pollution:addValue("London Station", {{2021, 03, 28}, {18, 20, 00}}, "PM9", 20, M4),
  M6 = pollution:addValue("London Station", {{2021, 03, 10}, {16, 20, 00}}, "PM10", 30, M5),
  M7 = pollution:addValue("London Station", {{2021, 03, 10}, {10, 20, 00}}, "temperature", 24, M6),
  M8 = pollution:addValue("London Station", {{2021, 03, 10}, {20, 20, 00}}, "PM9", 90, M7),

  ?assertMatch(3.0, pollution:getDailyAverageDataCount("London Station", M8)).


getDailyAverageDataCount1_test() ->
  M1 = pollution:createMonitor(),
  M2 = pollution:addStation("London Station", {10, 20}, M1),
  M3 = pollution:addValue("London Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 10, M2),
  M4 = pollution:addValue("London Station", {{2021, 03, 28}, {12, 20, 00}}, "temperature", 5, M3),
  M5 = pollution:addValue("London Station", {{2021, 03, 28}, {18, 20, 00}}, "PM9", 20, M4),
  M6 = pollution:addValue("London Station", {{2021, 03, 10}, {16, 20, 00}}, "PM10", 30, M5),
  M7 = pollution:addValue("London Station", {{2021, 03, 10}, {10, 20, 00}}, "temperature", 24, M6),
  M8 = pollution:addValue("London Station", {{2021, 03, 10}, {20, 20, 00}}, "PM9", 90, M7),

  M9 = pollution:addStation("Berlin Station", {50, 50}, M8),
  M10 = pollution:addValue("Berlin Station", {{2021, 03, 28}, {08, 20, 00}}, "PM10", 5, M9),
  M11 = pollution:addValue("Berlin Station", {{2021, 03, 29}, {12, 20, 00}}, "temperature", 15, M10),
  M12 = pollution:addValue("Berlin Station", {{2021, 03, 39}, {18, 20, 00}}, "PM9", 80, M11),

  ?assertMatch(2.0, pollution:getDailyAverageDataCount(M12)).
