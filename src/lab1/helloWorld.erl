%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. mar 2021 12:45
%%%-------------------------------------------------------------------
-module(helloWorld).
-author("Ola").

%% API
-export([power/2]).

power(Number1, 1) ->
  Number1;
power(Number1, Number2) ->
  Number1 * power(Number1, Number2 - 1).
