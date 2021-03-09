%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. mar 2021 11:01
%%%-------------------------------------------------------------------
-module(main).
-author("Ola").

%% API
-export([power/2]).

power(_, 0) ->
  1;
power(A, B) ->
  A * power(A, B - 1).

%% c(main).
%% main:power(2, 3).
