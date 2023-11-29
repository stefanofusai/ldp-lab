-module(echo).
-export([print/1, start/0, stop/0]).

print(Msg) ->
    case Server = whereis(server) of
        undefined -> error("the server hasn't been started yet~n");
        _ -> Server ! {print, Msg}
    end,
    ok.

start() ->
    Server = spawn(fun server:loop/0),
    register(server, Server),
    ok.

stop() ->
    Server = whereis(server),
    Server ! stop,
    ok.
