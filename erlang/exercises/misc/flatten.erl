-module(flatten).
-export([flatten/1]).

flatten([]) ->
    [];
flatten([Head | Tail]) ->
    Head ++ flatten(Tail).
