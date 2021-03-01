%%%-------------------------------------------------------------------
%%% @author Ola
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. mar 2021 18:13
%%%-------------------------------------------------------------------
-module(onp).
-author("Ola").

%% API
-export([onp/1, onp/2, convert/1]).

convert(String) ->
  case string:to_float(String) of
    {error, no_float} -> list_to_integer(String);
    {N, _} -> N
  end.

onp("") -> 0;
onp(Expression) ->
  try
    io:format("~w~n",onp(string:tokens(Expression, " "), []))
  catch
    error:badarith -> io:format("Error ~n")
  end.

onp([], Stack) -> Stack;
onp([Head | Tail], []) -> onp(Tail, [convert(Head)]);
onp(["+" | Tail], [First, Second | Stack]) -> onp(Tail, [Second + First | Stack]);
onp(["-" | Tail], [First, Second | Stack]) -> onp(Tail, [Second - First | Stack]);
onp(["*" | Tail], [First, Second | Stack]) -> onp(Tail, [Second * First | Stack]);
onp(["/" | Tail], [First, Second | Stack]) -> onp(Tail, [Second / First | Stack]);
onp(["sqrt" | Tail], [First | Stack]) -> onp(Tail, [math:sqrt(First) | Stack]);
onp(["pow" | Tail], [First, Second | Stack]) -> onp(Tail, [math:pow(Second, First) | Stack]);
onp(["sin" | Tail], [First | Stack]) -> onp(Tail, [math:sin(First) | Stack]);
onp(["cos" | Tail], [First | Stack]) -> onp(Tail, [math:cos(First) | Stack]);
onp(["tan" | Tail], [First | Stack]) -> onp(Tail, [math:tan(First) | Stack]);
onp([Head | Tail], Stack) -> onp(Tail, [convert(Head) | Stack]).


%%1 + 2 * 3 - 4 / 5 + 6 <=> 2 3 * 1 + 4 5 / - 6 +
%%1 + 2 + 3 + 4 + 5 + 6 * 7 <=> 1 2 + 3 + 4 + 5 + 6 7 * +
%%( (4 + 7) / 3 ) * (2 - 19) <=> 4 7 + 3 / 2 19 - *
%%17 * (31 + 4) / ( (26 - 15) * 2 - 22 ) - 1 <=> 17 31 4 + * 26 15 - 2 * 22 - / 1 -