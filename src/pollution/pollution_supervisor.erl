%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. kwi 2021 12:10
%%%-------------------------------------------------------------------
-module(pollution_supervisor).
-author("Ola").
-version('1.0').
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local,?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => 2,
    period => 5
  },

  ChildSpecs = [
    #{
      id => 'pollution_gen_server',
      start => {
        pollution_gen_server,
        start_link,
        []
      },
      restart => permanent,
      shutdown => 3000,
      type => worker,
      modules => [pollution_gen_server]
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.