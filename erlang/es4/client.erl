-module(client).
-export([start/0]).

run() ->
    case whereis(server) of
        undefined -> error("the server hasn't been started yet");
        _ -> loop()
    end.

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
    Server = spawn(fun server:loop/0),
    link(Server),
    try register(server, Server) of
        _ -> Server ! {print, "Successfully registered server"}
    catch
        error:badarg -> Server ! {print, "Server is already registered"}
    after
        Server !
            {print, lists:flatten(io_lib:format("Established connection with client ~p", [self()]))}
    end,
    run().

stop() ->
    exit(suicide),
    ok.
