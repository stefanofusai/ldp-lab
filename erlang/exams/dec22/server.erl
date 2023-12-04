-module(server).
-export([start/0]).

start() ->
    group_leader(whereis(user), self()),
    io:format("[server] Started @ ~p~n", [self()]),
    loop(undefined, []).

loop(OriginalLength, Acc) when OriginalLength == length(Acc) ->
    io:format("[server] The original list in reversed order is: ~p~n", [
        lists:reverse([
            Elem
         || {_, Elem} <- lists:sort(
                fun({Index1, _}, {Index2, _}) ->
                    % reversing the list could be also done here
                    % but for clarity I prefer calling lists:reverse
                    Index1 =< Index2
                end,
                Acc
            )
        ])
    ]),
    loop(undefined, []);
loop(OriginalLength, Acc) ->
    receive
        {from, From, {element, Elem, original_length, OriginalLengthNew}} ->
            io:format("[server] Received element ~p from middleman @ ~p~n", [Elem, From]),
            loop(OriginalLengthNew, [Elem | Acc]);
        {from, From, {stop, Reason}} ->
            io:format("[server] Received stop signal with reason `~p` from ~p~n", [Reason, From]);
        Other ->
            io:format("[server] Received unknown message: `~p`~n", [Other]),
            loop(OriginalLength, Acc)
    end.
