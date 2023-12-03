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
    io:format("[client] Shutting down processes on ~p nodes...~n", [length(nodes())]),
    rpc(server, {stop, shutdown}),
    rpc(mm1, {stop, shutdown}),
    rpc(mm2, {stop, shutdown}),
    io:format("[client] Successfully shut down processes on ~p nodes~n", [length(nodes())]).

do_reverse(List) ->
    ListLength = length(List),
    {List1Temp, List2} = lists:split(ListLength div 2, List),
    rpc(mm1, {set_start_index, 1}),
    case ListLength rem 2 of
        0 ->
            List1 = List1Temp,
            StartIndex2 = ListLength div 2,
            OriginalLength = ListLength;
        1 ->
            List1 = List1Temp ++ [hd(List2)],
            StartIndex2 = ListLength div 2 + 1,
            OriginalLength = ListLength + 1
    end,
    rpc(mm2, {set_start_index, StartIndex2}),
    rpc(mm1, {forward_list, List1, original_length, OriginalLength}),
    rpc(mm2, {forward_list, List2, original_length, OriginalLength}).

% Private functions

rpc(To, Msg) ->
    Pid = global:whereis_name(To),
    case is_pid(Pid) of
        true -> Pid ! {from, self(), Msg};
        false -> error(lists:flatten(io_lib:format("[client] Pid not found for `~p`", [To])))
    end.
