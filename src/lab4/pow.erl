%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. kwi 2021 11:15
%%%-------------------------------------------------------------------
-module(pow).
-author("Ola").
-behaviour(gen_server).


%% API
-export([start_link/0, step/0, read/0, close/0, crash/0, setValue/1]).
-export([init/1,handle_call/3,handle_cast/2,terminate/2, handle_info/2]).

%% START %%
start_link()   -> gen_server:start_link({local,?MODULE},?MODULE,2,[]).
init(N)        -> {ok,N}.

%% INTERFACE CLIENT -> SERVER %%
step()            -> gen_server:cast(?MODULE,step).
read()            -> gen_server:call(?MODULE,read).
close()           -> gen_server:call(?MODULE,terminate).
crash()           -> gen_server:cast(?MODULE,crash).
setValue(Value)   -> gen_server:call(?MODULE, {setValue, Value}).

%% HANDLE MESSAGES %%
handle_cast(step, N) -> {noreply, N*N};
handle_cast(crash, N) -> no:exist(), {noreply, N}.

handle_call(read,_From, N)      -> {reply, N, N};
handle_call(terminate,_From,N) -> {stop, normal, ok, N};
handle_call({setValue, Value}, _From, _) -> {reply, Value, Value}.

handle_info(Message, N) -> {noreply, N}.

terminate(normal, N) -> io:format("The number is: ~B~nBye.~n",[N]), ok.
