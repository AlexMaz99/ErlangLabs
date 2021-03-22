%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. mar 2021 12:45
%%%-------------------------------------------------------------------
-module(functions).
-author("Ola").

%% API
-export([map/2, filter/2, digitsSum/1]).

map(Fun, List) ->
  [Fun(X) || X <- List].
filter(Fun, List) ->
  [X || X <- List, Fun(X)].

%% functions:map(fun(X) -> 2 * X end, [1, 2, 3, 4]).
%% functions:filter(fun(X) -> X rem 2 == 0 end, [1, 2, 3, 4]).

digitsSum(X) ->
  lists:foldl(fun (A, B) -> A + B -$0 end, 0, integer_to_list(X)).

%% $0 - extracting the numerical value from digit symbol
%% lists:filter(fun (X) -> functions:digitsSum(X) rem(3) == 0 end, [rand:uniform(25) || _ <- lists:seq(1, 1000000)]).