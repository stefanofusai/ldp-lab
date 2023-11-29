-module(client).
-export([start/0]).

loop() ->
    Server = whereis(server),
    Server !
        {print,
            "I would do much more if this dumb server would let me do more than just printing stuff..."},
    Server !
        {print, "I guess I'll just think about my life for about 500ms and decide what to do"},
    timer:sleep(500),
    case N = rand:uniform() of
        N when N =< 0.1 ->
            Server ! {print, "I'm done with this!"},
            stop();
        _ ->
            Server ! {print, "Let me give this another go..."},
            loop()
    end.

start() ->
    case whereis(server) of
        undefined ->
            Server = spawn(fun server:loop/0),
            register(server, Server),
            Server ! {print, "Spawned and registered server"};
        Server ->
            Server ! {print, "Connected to existing server"}
    end,
    loop().

stop() ->
    exit(suicide),
    ok.
