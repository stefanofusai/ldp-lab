-module(avg_server).
-export([loop/1, start/0, start/1]).

avg(List) ->
    avg(0, length(List), List).
avg(Acc, Len, []) ->
    Acc / Len;
avg(Acc, Len, [H | T]) ->
    avg(Acc + H, Len, T).

loop(List) ->
    receive
        X when is_number(X) ->
            ListNew = lists:reverse([X | List]),
            io:format("The average of ~p is ~p~n", [ListNew, avg(ListNew)]),
            loop(ListNew);
        Other ->
            io:format("Invalid input: ~p~n", [Other]),
            loop(List)
    end.

start() ->
    start([]).
start(List) ->
    spawn(?MODULE, loop, [List]).
