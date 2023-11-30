-module(server).
-export([start/0]).

start() ->
    global:register_name(server, self()),
    group_leader(whereis(user), self()),
    io:format("[server] Started @ ~p~n", [self()]),
    loop().

loop() ->
    receive
        {from, From, element, Elem} ->
            io:format("[server] Received element ~p from ~p", [Elem, From])
    end,
    loop().
