-module(es3).
-export([init/0, start/3]).

init() ->
    receive
        {From, set_pid_next, PidNext} ->
            io:format("[~p] (caller: ~p) set_pid_next: ~p~n", [self(), From, PidNext]),
            loop(PidNext)
    end.
loop(PidNext) ->
    receive
        {From, send_message, Message, MMax} ->
            io:format("[~p] (caller: ~p) send_message to ~p (1/~p messages sent): ~p~n", [
                self(), From, PidNext, MMax, Message
            ]),
            PidNext ! {self(), send_message, Message, MMax, 1},
            loop(PidNext);
        {From, send_message, Message, MMax, MCount} when MCount =< MMax ->
            io:format("[~p] (caller: ~p) send_message to ~p (~p/~p messages sent): ~p~n", [
                self(), From, PidNext, MCount, MMax, Message
            ]),
            PidNext ! {self(), send_message, Message, MMax, MCount + 1},
            loop(PidNext);
        {From, send_message, Message, MMax, MCount} ->
            io:format(
                "[~p] (caller: ~p) send_message to ~p refused (~p/~p messages sent): ~p~n", [
                    self(), From, PidNext, MCount, MMax, Message
                ]
            ),
            loop(PidNext);
        {From, quit} ->
            io:format("[~p] (caller: ~p) quit~n", [self(), From]),
            PidNext ! {self(), quit}
    end.

set_pids_next(PidFirst, [PidLast]) ->
    PidLast ! {self(), set_pid_next, PidFirst};
set_pids_next(PidFirst, [Pid | T = [PidNext | _]]) ->
    Pid ! {self(), set_pid_next, PidNext},
    set_pids_next(PidFirst, T).

start(M, N, Message) ->
    Pids = [spawn(?MODULE, init, []) || _ <- lists:seq(1, N)],
    PidFirst = hd(Pids),
    set_pids_next(PidFirst, Pids),
    io:format("[~p] Sleeping for 5000ms before starting ring...~n", [self()]),
    timer:sleep(5000),
    PidFirst ! {self(), send_message, Message, M},
    io:format("[~p] Sleeping for 5000ms before closing ring...~n", [self()]),
    timer:sleep(5000),
    PidFirst ! {self(), quit}.
