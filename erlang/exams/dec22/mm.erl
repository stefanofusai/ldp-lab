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
        List when is_list(List) ->
            io:format("[~p] Got list ~p from client~n", [Name, List]),
            lists:foreach(
                fun(Elem) ->
                    io:format("[~p] Sending element ~p to server~n", [Name, Elem]),
                    Server ! {from, self(), element, Elem}
                end,
                List
            )
    end,
    loop(Name).
