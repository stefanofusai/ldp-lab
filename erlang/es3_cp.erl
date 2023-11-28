-module(es3_cp).
-export([init/2, start/3]).

init(N, C) ->
    receive
        {From, set_pid_next, PidNext} ->
            io:format("[~p] I am the ~p/~p child process, created by ~p and linked to ~p~n", [
                self(), C, N, From, PidNext
            ]),
            loop(PidNext)
    end.
loop(PidNext) ->
    receive
        {From, send_message, Message, MMax, MCount} when MCount =< MMax ->
            io:format("[~p] Received 'send_message' signal from ~p (~p/~p): sending ~p to ~p~n", [
                self(), From, MCount, MMax, Message, PidNext
            ]),
            PidNext ! {self(), send_message, Message, MMax, MCount + 1},
            loop(PidNext);
        {From, send_message, _, MMax, MCount} ->
            io:format("[~p] Received 'send_message' signal from ~p (~p/~p): ignoring signal~n", [
                self(), From, MCount, MMax
            ]),
            loop(PidNext);
        {From, quit} ->
            io:format("[~p] Received 'quit' signal from ~p: quitting~n", [self(), From]),
            PidNext ! {self(), quit}
    end.

set_pids_next(PidFirst, [PidLast]) ->
    PidLast ! {self(), set_pid_next, PidFirst};
set_pids_next(PidFirst, [Pid | T = [PidNext | _]]) ->
    Pid ! {self(), set_pid_next, PidNext},
    set_pids_next(PidFirst, T).

start(M, N, Message) ->
    io:format("[Main] Spawning processes...~n"),
    Pids = [spawn(?MODULE, init, [N, C]) || C <- lists:seq(1, N)],
    PidFirst = hd(Pids),
    set_pids_next(PidFirst, Pids),
    io:format("[Main] Sending 'send_message' signal in 5000ms...~n"),
    timer:sleep(5000),
    PidFirst ! {self(), send_message, Message, M, 1},
    io:format("[Main] Sending 'quit' signal in 5000ms...~n"),
    timer:sleep(5000),
    PidFirst ! {self(), quit}.
