-module(filter).
-export([filter/2, lc/2]).

filter(Pred, List) ->
    lists:filter(Pred, List).

lc(Pred, List) ->
    [X || X <- List, Pred(X)].
