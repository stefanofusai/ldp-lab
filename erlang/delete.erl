-module(delete).
-export([delete/2]).

delete(_, []) ->
    [];
delete(X, [X | Tail]) ->
    Tail;
delete(X, [Head | Tail]) ->
    [Head | delete(X, Tail)].
