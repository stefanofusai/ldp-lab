-module(mm).
-export([start/1]).

start(Name) ->
    global:register_name(Name, self()),
    group_leader(whereis(user), self()),
    io:format("[~p] Started @ ~p~n", [Name, self()]),
    loop(Name).

loop(Name) ->
    Server = global:whereis_name(server),
    receive
        {from, From, forward_list, List} when is_list(List) ->
            io:format("[~p] Got list ~p from ~p~n", [Name, List, From]),
            lists:foreach(
                fun(Elem) ->
                    io:format("[~p] Sending element ~p to server~p~n", [Name, Elem, Server]),
                    Server ! {from, self(), element, Elem}
                end,
                List
            ),
            loop(Name);
        {from, From, stop, Reason} ->
            io:format("[~p] Received stop signal ~p from ~p~n", [Name, Reason, From]);
        Other ->
            io:format("[server] Received unknown message ~p~n", [Other]),
            loop(Name)
    end.
