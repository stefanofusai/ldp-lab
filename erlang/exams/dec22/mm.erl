-module(mm).
-export([start/1]).

start(Name) ->
    global:register_name(Name, self()),
    group_leader(whereis(user), self()),
    io:format("[~p] Started @ ~p~n", [Name, self()]),
    loop(Name).

loop(Name) ->
    receive
        {from, From, {set_index, Index}} ->
            io:format("[~p] Received set_index signal with index `~p` from client @ ~p~n", [
                Name, Index, From
            ]),
            loop(Name, Index);
        Other ->
            io:format("[~p] Received unknown message: `~p`~n", [Name, Other]),
            loop(Name)
    end.

loop(Name, Index) ->
    Server = global:whereis_name(server),
    case is_pid(Server) of
        true -> ok;
        false -> exit("[~p] Pid not found for `server`~n", [Name])
    end,
    receive
        {from, From, {forward_list, List, original_length, OriginalLength}} when is_list(List) ->
            io:format("[~p] Received list ~p from client @ ~p~n", [Name, List, From]),
            lists:foreach(
                fun({ElemIndex, Elem}) ->
                    io:format("[~p] Sending element `~p` to server @ ~p~n", [Name, Elem, Server]),
                    Server !
                        {from, self(),
                            {element, {Index + ElemIndex - 1, Elem}, original_length,
                                OriginalLength}}
                end,
                lists:enumerate(List)
            ),
            loop(Name, Index);
        {from, From, {stop, Reason}} ->
            io:format("[~p] Received stop signal with reason `~p` from ~p~n", [Name, Reason, From]);
        Other ->
            io:format("[~p] Received unknown message: `~p`~n", [Name, Other]),
            loop(Name, Index)
    end.
