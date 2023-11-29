-module(server).
-export([loop/0]).

loop() ->
    receive
        {print, Msg} ->
            io:format("~p~n", [Msg]),
            loop();
        stop ->
            io:format("exit signal received")
    end.
