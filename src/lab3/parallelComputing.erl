%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. kwi 2021 14:11
%%%-------------------------------------------------------------------
-module(parallelComputing).
-author("Ola").

%% API
-export([
  getLocations/2,
  dist/2,
  findMyLocker/2,
  findMinDistancesSequential/2,
  findMinDistancesParallel/2,
  findMinDistanceParallel/2,
  collectData/4,
  findMinDistancesSemiParallel/2,
  findSomeMinDistancesSemiParallel/2
]).


getLocations(LockersNumber, PeopleNumber) ->
  LockerLocations = [{X, Y} || X <- qsort:randomElems(LockersNumber, 0, 10), Y <- qsort:randomElems(LockersNumber, 0, 10)],
  PeopleLocations = [{X, Y} || X <- qsort:randomElems(PeopleNumber, 0, 10), Y <- qsort:randomElems(PeopleNumber, 0, 10)],
  {LockerLocations, PeopleLocations}.


dist({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).


findMyLocker(PersonLocation, LockerLocations) ->
  Distances = [{dist(PersonLocation, LockerLocation), {PersonLocation, LockerLocation}} || LockerLocation <- LockerLocations],
  lists:min(Distances).


%% SEQUENTIAL
findMinDistancesSequential(PeopleLocations, LockerLocations) ->
  {_, StartTime, _} = now(),
  Result = [findMyLocker(Person, LockerLocations) || Person <- PeopleLocations],
  {_, EndTime, _} = now(),
  io:format("Sequential time: ~p seconds ~n", [EndTime - StartTime]).


%% FULL PARALLEL
findMinDistanceParallel(PersonLocation, LockerLocations) ->
  fullParallel ! findMyLocker(PersonLocation, LockerLocations).


findMinDistancesParallel(PeopleLocations, LockerLocations) ->
  {_, StartTime, _} = now(),
  Pid = spawn(fun() -> collectData(0, length(PeopleLocations), [], StartTime) end),
  register(fullParallel, Pid),
  [spawn(fun() -> findMinDistanceParallel(PersonLocation, LockerLocations) end) || PersonLocation <- PeopleLocations].


%% SEMI PARALLEL
findSomeMinDistancesSemiParallel(PeopleLocations, LockerLocations) ->
  [semiParallel ! findMyLocker(PersonLocation, LockerLocations) || PersonLocation <- PeopleLocations].


findMinDistancesSemiParallel(PeopleLocations, LockerLocations) ->
  {_, StartTime, _} = now(),
  Pid = spawn(fun() -> collectData(0, length(PeopleLocations), [], StartTime) end),
  register(semiParallel, Pid),
  {Part12, Part34} = lists:split(trunc(length(PeopleLocations) / 2), PeopleLocations),
  {Part1, Part2} = lists:split(trunc(length(Part12) / 2), Part12),
  {Part3, Part4} = lists:split(trunc(length(Part34) / 2), Part34),
  spawn(fun() -> findSomeMinDistancesSemiParallel(Part1, LockerLocations) end),
  spawn(fun() -> findSomeMinDistancesSemiParallel(Part2, LockerLocations) end),
  spawn(fun() -> findSomeMinDistancesSemiParallel(Part3, LockerLocations) end),
  spawn(fun() -> findSomeMinDistancesSemiParallel(Part4, LockerLocations) end).


collectData(ReceivedNumber, AllNumber, Result,  StartTime) when ReceivedNumber == AllNumber ->
  {_, EndTime, _} = now(),
  io:format("Result: ~p ~n", [Result]),
  io:format("Full parallel time: ~p seconds ~n", [EndTime - StartTime]);


collectData(ReceivedNumber, AllNumber, Result, StartTime) ->
  receive
    Data -> collectData(ReceivedNumber + 1, AllNumber, [Data | Result], StartTime)
  end.