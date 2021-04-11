%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. kwi 2021 12:35
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Ola").

%% API
-export([start/0, stop/0, play/1, ping/1, pong/1]).

start() ->
  register(ping, spawn(fun() -> ping(0) end)),
  register(pong, spawn(fun() -> pong(0) end)).

stop() ->
  ping ! stop,
  pong ! stop.

play(N) ->
  ping ! N.

ping(Sum) ->
  receive
    stop ->
      io:format("Ping: stop ~n"),
      ok;
    0 ->
      io:format("Ping: total ~p ~n", [Sum]),
      pong ! total,
      ping(Sum);
    N when N > 0 ->
      io:format("Ping: ~p ~n", [N - 1]),
      timer:sleep(1000),
      pong ! (N - 1),
      ping(Sum + 1)
  after 20000 ->
    io:format("Ping: timeout ~n"),
    ok
  end.

pong(Sum) ->
  receive
    stop ->
      io:format("Pong: stop ~n"),
      ok;
    total ->
      io:format("Pong: total ~p ~n", [Sum]),
      pong(Sum);
    N ->
      io:format("Pong: ~p ~n", [N]),
      timer:sleep(1000),
      ping ! N,
      pong(Sum + 1)
  after 20000 ->
    io:format("Pong: timeout ~n"),
    ok
  end.