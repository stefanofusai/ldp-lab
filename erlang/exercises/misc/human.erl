-module(human).
-export([live/0]).

live() ->
    receive
        {From, walk} -> From ! io:format("Walk? Sure can do!~n");
        {From, fly} -> From ! io:format("Fly? No can do!~n");
        {From, Other} -> From ! io:format("~s? Maybe can do!~n", [Other])
    end,
    live().
