-module(stack).
-export([exists/2, delete/2, length/1, push/2, pop/1, run/1, start/0]).

% Main methods

run(List) ->
    receive
        {From, {exists, Elem}} ->
            Msg =
                case lists:member(Elem, List) of
                    true -> ok;
                    false -> not_found
                end,
            From ! {self(), Msg},
            run(List);
        {From, {delete, Elem}} ->
            {ListNew, Msg} =
                case lists:member(Elem, List) of
                    true -> {lists:delete(Elem, List), ok};
                    false -> {List, not_found}
                end,
            From ! {self(), Msg},
            run(ListNew);
        {From, {push, Elem}} ->
            From ! {self(), ok},
            run([Elem | List]);
        {From, {pop}} ->
            {ListNew, Msg} =
                case List of
                    [] -> {[], stack_empty};
                    [Head | Tail] -> {Tail, Head}
                end,
            From ! {self(), Msg},
            run(ListNew);
        {From, {length}} ->
            From ! {self(), erlang:length(List)},
            run(List)
    end.

start() ->
    spawn(?MODULE, run, [[]]).

% Interface methods

delete(PID, Elem) ->
    PID ! {self(), {delete, Elem}},
    receive
        {PID, Msg} -> Msg
    end.

exists(PID, Elem) ->
    PID ! {self(), {exists, Elem}},
    receive
        {PID, Msg} -> Msg
    end.

length(PID) ->
    PID ! {self(), {length}},
    receive
        {PID, Msg} -> Msg
    end.

push(PID, Elem) ->
    PID ! {self(), {push, Elem}},
    receive
        {PID, Msg} -> Msg
    end.

pop(PID) ->
    PID ! {self(), {pop}},
    receive
        {PID, Msg} -> Msg
    end.
