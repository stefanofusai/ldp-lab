-module(client).
-export([start/0, close/0, do_reverse/1]).

% Public functions

start() ->
    {ok, HostName} = inet:gethostname(),
    Server = spawn(list_to_atom("server@" ++ HostName), server, start, []),
    io:format("[client] Initialized server @ ~p~n", [Server]),
    MM1 = spawn(list_to_atom("mm1@" ++ HostName), mm, start, [mm1]),
    io:format("[client] Initialized mm1 @ ~p~n", [MM1]),
    MM2 = spawn(list_to_atom("mm2@" ++ HostName), mm, start, [mm2]),
    io:format("[client] Initialized mm2 @ ~p~n", [MM2]),
    io:format("[client] Startup successful on ~p nodes~n", [length(nodes())]),
    ok.

close() ->
    io:format("[client] Shutting down processes on ~p nodes~n...", [length(nodes())]),
    rpc(server, {stop, shutdown}),
    rpc(mm1, {stop, shutdown}),
    rpc(mm2, {stop, shutdown}),
    io:format("[client] Successfully shut down processes on ~p nodes~n", [length(nodes())]).

% TODO
do_reverse(List) ->
    rpc(mm1, {forward_list, List}),
    rpc(mm2, {forward_list, List}).

% Private functions

rpc(To, Msg) ->
    {MsgCode, MsgContent} = Msg,
    global:whereis_name(To) ! {from, self(), MsgCode, MsgContent}.
