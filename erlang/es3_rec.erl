-module(es3_rec).
-export([init/3, init/4, start/3]).

init(M, N, C) ->
    PidNext = spawn(?MODULE, init, [M, N, C + 1, self()]),
    io:format("[~p] I am the first child process, linked to ~p~n", [self(), PidNext]),
    loop(M, N, PidNext).

init(M, N, C, PidFirst) when C < N ->
    PidNext = spawn(?MODULE, init, [M, N, C + 1, PidFirst]),
    io:format("[~p] I am the ~p/~p child process, linked to ~p~n", [self(), C, N, PidNext]),
    loop(M, N, PidNext);
init(M, N, C, PidFirst) when C == N ->
    io:format("[~p] I am the last child process, linked to ~p~n", [self(), PidFirst]),
    loop(M, N, PidFirst).

loop(M, N, PidNext) ->
    receive
        {From, send_message, Message, MMax, MCount} when MCount =< MMax ->
            io:format("[~p] Received 'send_message' signal from ~p (~p/~p): sending ~p to ~p~n", [
                self(), From, MCount, MMax, Message, PidNext
            ]),
            PidNext ! {self(), send_message, Message, MMax, MCount + 1},
            loop(M, N, PidNext);
        {From, send_message, _, MMax, MCount} ->
            io:format("[~p] Received 'send_message' signal from ~p (~p/~p): ignoring signal~n", [
                self(), From, MCount, MMax
            ]),
            loop(M, N, PidNext);
        {From, quit} ->
            io:format("[~p] Received 'quit' signal from ~p: quitting~n", [self(), From]),
            PidNext ! {self(), quit}
    end.

start(M, N, Message) ->
    io:format("[Main] Spawning first process...~n"),
    Pid = spawn(?MODULE, init, [M, N, 1]),
    io:format("[Main] Sending 'send_message' signal in 5000ms...~n"),
    timer:sleep(5000),
    Pid ! {self(), send_message, Message, M, 1},
    io:format("[Main] Sending 'quit' signal in 5000ms...~n"),
    timer:sleep(5000),
    Pid ! {self(), quit}.
