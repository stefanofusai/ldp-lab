-module(joseph).
-export([joseph/2]).

joseph(N, K) ->
    io:format("In a circle of ~p people, killing number ~p~n", [N, K]),

    Hebrews =
        [HebrewsFirst | HebrewsTail] = [
            spawn(hebrew, start, [self(), N, K, Position])
         || Position <- lists:seq(1, N)
        ],
    lists:foreach(
        fun({Hebrew, Neighbor}) ->
            Hebrew ! {set_neighbor, Neighbor}
        end,
        lists:zip(Hebrews, HebrewsTail ++ [HebrewsFirst])
    ),
    % By sending this message to the first Hebrew (HebrewsFirst), the cycle of counting and elimination begins.
    % The 'die' message will circulate through the circle, with each Hebrew deciding whether to pass it on or eliminate themselves based on the counter_n and counter_k values.
    % The process continues until the conditions in the loop function of the hebrew module are met for identifying the survivor (Joseph).
    HebrewsFirst ! {die, from, lists:last(Hebrews), counter_n, 1, counter_k, 1},

    receive
        {im_joseph, Position} ->
            io:format("Joseph is the Hebrew in position ~p~n", [Position]);
        Other ->
            io:format("Invalid message: ~p", [Other])
    end.
