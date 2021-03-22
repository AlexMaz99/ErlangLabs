%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. mar 2021 12:45
%%%-------------------------------------------------------------------
-module(qsort).
-author("Ola").

%% API
-export([lessThan/2, grtEqThan/2, qs/1, randomElems/3, compareSpeeds/3]).

lessThan(List, Arg) ->
  [X || X <- List, X < Arg].
grtEqThan(List, Arg) ->
  [X || X <- List, X >= Arg].

qs([]) ->
  [];
qs([Pivot|Tail]) ->
  qs(lessThan(Tail,Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail,Pivot)).

randomElems(N, Min, Max) ->
  [Min + rand:uniform(Max - Min) || _ <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  io:format("First function's time ~p~n", [Time1]),
  io:format("Second function's time ~p~n", [Time2]).

%% qsort:compareSpeeds(qsort:randomElems(100000, 1, 500), fun qsort:qs/1, fun lists:sort/1).
