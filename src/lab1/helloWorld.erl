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

%% P1 = {"London Station", 2, [{pm10, 123}, {pm2_5, 3.4}]}.
%% P2 = {"Warsaw Station", 2.5, [{pm10, 12}]}.
%% P3 = {"Krakow Station", 1, [{pm10, 23}, {pm2_5, 0.4}, {pm30, 21.5}]}.
%%
%% ListaPomiarow = [P1, P2, P3].
%%
%% P4 = {"New York Station", 3, [{pm2_5, 3.4}]}.
%%
%% NowaListaPomiarow = [P4 | ListaPomiarow].
%% NowaListaPomiarow = [P4] ++ ListaPomiarow.
%% NLP = ListaPomiarow ++ [P4]
%% NowszaListaPomiarow = [P4, P4 | ListaPomiarow].
%%
%% [El, El2 | _] = NowszaListaPomiarow.
%% {NazwaP1, _, _} = P1.