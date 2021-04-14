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
  collectData/3,
  findMinDistancesSemiParallel/2,
  findSomeMinDistancesSemiParallel/2,
  compareSpeeds/2
]).


getLocations(PeopleNumber, LockersNumber) ->
  LockerLocations = [{X, Y} || X <- qsort:randomElems(LockersNumber, 0, 10), Y <- qsort:randomElems(LockersNumber, 0, 10)],
  PeopleLocations = [{X, Y} || X <- qsort:randomElems(PeopleNumber, 0, 10), Y <- qsort:randomElems(PeopleNumber, 0, 10)],
  {PeopleLocations, LockerLocations}.


dist({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).


findMyLocker(PersonLocation, LockerLocations) ->
  Distances = [{dist(PersonLocation, LockerLocation), {PersonLocation, LockerLocation}} || LockerLocation <- LockerLocations],
  lists:min(Distances).


%% SEQUENTIAL
findMinDistancesSequential(PeopleLocations, LockerLocations) ->
  Result = lists:min([findMyLocker(PersonLocation, LockerLocations) || PersonLocation <- PeopleLocations]),
  io:format("Result: ~p~n", [Result]).


%% FULL PARALLEL
findMinDistanceParallel(PersonLocation, LockerLocations) ->
  fullParallel ! findMyLocker(PersonLocation, LockerLocations).


findMinDistancesParallel(PeopleLocations, LockerLocations) ->
  register(fullParallel, spawn(fun() -> collectData(0, length(PeopleLocations), []) end)),
  [spawn(fun() -> findMinDistanceParallel(PersonLocation, LockerLocations) end) || PersonLocation <- PeopleLocations].


%% SEMI PARALLEL
findSomeMinDistancesSemiParallel(PeopleLocations, LockerLocations) ->
  [semiParallel ! findMyLocker(PersonLocation, LockerLocations) || PersonLocation <- PeopleLocations].


findMinDistancesSemiParallel(PeopleLocations, LockerLocations) ->
  register(semiParallel, spawn(fun() -> collectData(0, length(PeopleLocations), []) end)),
  {Part12, Part34} = lists:split(trunc(length(PeopleLocations) / 2), PeopleLocations),
  {Part1, Part2} = lists:split(trunc(length(Part12) / 2), Part12),
  {Part3, Part4} = lists:split(trunc(length(Part34) / 2), Part34),
  spawn(fun() -> findSomeMinDistancesSemiParallel(Part1, LockerLocations) end),
  spawn(fun() -> findSomeMinDistancesSemiParallel(Part2, LockerLocations) end),
  spawn(fun() -> findSomeMinDistancesSemiParallel(Part3, LockerLocations) end),
  spawn(fun() -> findSomeMinDistancesSemiParallel(Part4, LockerLocations) end).


collectData(ReceivedNumber, AllNumber, Result) when ReceivedNumber == AllNumber ->
  io:format("Result: ~p ~n", [lists:min(Result)]);


collectData(ReceivedNumber, AllNumber, Result) ->
  receive
    Data -> collectData(ReceivedNumber + 1, AllNumber, [Data | Result])
  end.


%% COMPARE
compareSpeeds(PeopleNumber, LockersNumber) ->
  {PeopleLocations, LockerLocations} = getLocations(PeopleNumber, LockersNumber),
  {TimeSeq, _} = timer:tc(fun() -> findMinDistancesSequential(PeopleLocations, LockerLocations) end),
  {TimeParallel, _} = timer:tc(fun() -> findMinDistancesParallel(PeopleLocations, LockerLocations) end),
  {TimeSemiParallel, _} = timer:tc(fun() -> findMinDistancesSemiParallel(PeopleLocations, LockerLocations) end),
  io:format("Sequential time: ~p us ~n", [TimeSeq]),
  io:format("Parallel time: ~p us ~n", [TimeParallel]),
  io:format("Semi Parallel time: ~p us ~n", [TimeSemiParallel]).