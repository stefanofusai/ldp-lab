-module(stack).
-export([run/1]).

run(List) ->
    receive
        {From, {exists, Elem}} ->
            Msg =
                case lists:member(Elem, List) of
                    true -> ok;
                    false -> not_found
                end,
            From ! {self(), Msg};
        {From, {push, Elem}} ->
            From ! {self(), ok},
            run([Elem | List]);
        {From, {pop}} ->
            [Head | Tail] = List,
            From ! {self(), Head},
            run(Tail);
        {From, {length}} ->
            From ! {self(), lists:length(List)}
    end.
