%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. mar 2021 17:36
%%%-------------------------------------------------------------------
-module(myLists).
-author("Ola").

%% API
-export([contains/2, duplicateElements/1, duplicateElements/2, sumFloats/1, sumFloats/2]).

contains([], _) ->
  false;
contains([Head | Tail], Value)  ->
  (Head == Value) orelse contains(Tail, Value).

duplicateElements(List) ->
  duplicateElements(List, []).
duplicateElements([], Acc) ->
  Acc;
duplicateElements([Head | Tail], Acc) ->
  duplicateElements(Tail, Acc ++ [Head, Head]).

sumFloats(List) ->
  sumFloats(List, 0.0).
sumFloats([], Sum) ->
  Sum;
sumFloats([Head | Tail], Sum) when is_float(Head)->
  sumFloats(Tail, Sum + Head);
sumFloats([_ | Tail], Sum) ->
  sumFloats(Tail, Sum).
