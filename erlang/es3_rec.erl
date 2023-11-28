-module(es3_rec).
-export([init/4, init/5, start/3]).

init(M, N, C, Message) ->
    PidNext = spawn(?MODULE, init, [M, N, C + 1, Message, self()]),
    io:format("[~p] Spawned child process (1/~p processes spawned): ~p~n", [self(), N, PidNext]),
    loop(M, N, Message, PidNext).

init(M, N, C, Message, PidFirst) when C < N ->
    PidNext = spawn(?MODULE, init, [M, N, C + 1, Message, PidFirst]),
    io:format("[~p] Spawned child process (~p/~p processes spawned): ~p~n", [
        self(), C + 1, N, PidNext
    ]),
    loop(M, N, Message, PidNext);
init(M, N, C, Message, PidFirst) when C == N ->
    io:format("[~p] Spawned final child process~n", [self()]),
    loop(M, N, Message, PidFirst).

loop(M, N, Message, PidNext) ->
    receive
        {From, send_message, Message, MMax} ->
            io:format("[~p] (caller: ~p) send_message to ~p (1/~p messages sent): ~p~n", [
                self(), From, PidNext, MMax, Message
            ]),
            PidNext ! {self(), send_message, Message, MMax, 1},
            loop(M, N, Message, PidNext);
        {From, send_message, Message, MMax, MCount} when MCount =< MMax ->
            io:format("[~p] (caller: ~p) send_message to ~p (~p/~p messages sent): ~p~n", [
                self(), From, PidNext, MCount, MMax, Message
            ]),
            PidNext ! {self(), send_message, Message, MMax, MCount + 1},
            loop(M, N, Message, PidNext);
        {From, send_message, Message, MMax, MCount} ->
            io:format(
                "[~p] (caller: ~p) send_message to ~p refused (~p/~p messages sent): ~p~n", [
                    self(), From, PidNext, MCount, MMax, Message
                ]
            ),
            loop(M, N, Message, PidNext);
        {From, quit} ->
            io:format("[~p] (caller: ~p) quit~n", [self(), From]),
            PidNext ! {self(), quit}
    end.

start(M, N, Message) ->
    io:format("[~p] Starting ring...~n", [self()]),
    Pid = spawn(?MODULE, init, [M, N, 1, Message]),
    io:format("[~p] Sleeping for 5000ms before sending message...~n", [self()]),
    timer:sleep(5000),
    Pid ! {self(), send_message, Message, M},
    io:format("[~p] Sleeping for 5000ms before closing ring...~n", [self()]),
    timer:sleep(5000),
    Pid ! {self(), quit}.
