-module(client).
-export([start/0, close/0]).

start() ->
    {ok, HostName} = inet:gethostname(),
    Server = spawn_link(list_to_atom("server@" ++ HostName), server, start, []),
    io:format("[client] Successfully initialized server @ ~p~n", [Server]),
    MM1 = spawn_link(list_to_atom("mm1@" ++ HostName), mm, start, [mm1]),
    io:format("[client] Successfully initialized mm1 @ ~p~n", [MM1]),
    MM2 = spawn_link(list_to_atom("mm2@" ++ HostName), mm, start, [mm2]),
    io:format("[client] Successfully initialized mm2 @ ~p~n", [MM2]),
    io:format("[client] Startup successful on ~p nodes~n", [length(nodes())]),
    ok.

close() ->
    io:format("[client] Shutting down processes on ~p nodes~n...", [length(nodes())]),
    exit(shutdown),
    io:format("[client] Successfully shut down processes. ~p nodes left open~n", [length(nodes())]).
