-module(square).
-export([tr/1, lc/1, map/1]).

tr(Acc, []) ->
    lists:reverse(Acc);
tr(Acc, [Head | Tail]) ->
    tr([Head * Head | Acc], Tail).
tr(List) ->
    tr([], List).

lc(List) ->
    [X * X || X <- List].
map(List) ->
    lists:map(fun(X) -> X * X end, List).
