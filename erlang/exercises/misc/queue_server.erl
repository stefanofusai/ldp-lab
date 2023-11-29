-module(queue_server).
-export([init/0, loop/1, pop/1, push/2]).

% Main functions

init() ->
    spawn(?MODULE, loop, [[]]).

loop(Queue) ->
    receive
        {From, pop} ->
            case Queue of
                [] ->
                    QueueNew = Queue,
                    From ! {self(), error_empty_queue};
                [Head | QueueNew] ->
                    From ! {self(), Head}
            end;
        {From, push, Elem} ->
            QueueNew = Queue ++ [Elem],
            From ! {self(), ok};
        {From, _} ->
            QueueNew = Queue,
            From ! {self(), error_invalid_verb}
    end,
    loop(QueueNew).

% API functions

pop(Pid) ->
    Pid ! {self(), pop},
    receive
        {Pid, Resp} -> Resp
    end.

push(Pid, Elem) ->
    Pid ! {self(), push, Elem},
    receive
        {Pid, Resp} -> Resp
    end.
