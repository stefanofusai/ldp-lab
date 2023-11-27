-module(find).
-export([find/2]).

find(_, []) ->
    not_found;
find(X, [X | _]) ->
    {found, X};
find(X, [_ | Tail]) ->
    find(X, Tail).
