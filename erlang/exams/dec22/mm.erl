-module(mm).
-export([start/1]).

start(Name) ->
    group_leader(whereis(user), self()),
    io:format("[~p] Started @ ~p~n", [Name, self()]),
    loop(Name).

loop(Name) ->
    receive
        {from, From, {set_start_index, StartIndex}} ->
            io:format("[~p] Received set_start_index signal with index `~p` from client @ ~p~n", [
                Name, StartIndex, From
            ]),
            loop(Name, StartIndex);
        Other ->
            io:format("[~p] Received unknown message: `~p`~n", [Name, Other]),
            loop(Name)
    end.

loop(Name, StartIndex) ->
    Server = global:whereis_name(server),
    case is_pid(Server) of
        true -> ok;
        false -> error(lists:flatten(io_lib:format("[~p] Pid not found for `server`", [Name])))
    end,
    receive
        {from, From, {forward_list, List, original_length, OriginalLength}} when is_list(List) ->
            io:format("[~p] Received list ~p from client @ ~p~n", [Name, List, From]),
            lists:foreach(
                fun({ElemIndex, Elem}) ->
                    io:format("[~p] Sending element `~p` to server @ ~p~n", [Name, Elem, Server]),
                    Server !
                        {from, self(),
                            {element, {StartIndex + ElemIndex, Elem}, original_length,
                                OriginalLength}}
                end,
                lists:enumerate(List)
            ),
            loop(Name, StartIndex);
        {from, From, {stop, Reason}} ->
            io:format("[~p] Received stop signal with reason `~p` from ~p~n", [Name, Reason, From]);
        Other ->
            io:format("[~p] Received unknown message: `~p`~n", [Name, Other]),
            loop(Name, StartIndex)
    end.
