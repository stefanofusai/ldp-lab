-module(hebrew).
-export([start/4]).

start(Joseph, N, K, Position) ->
    receive
        {set_neighbor, Neighbor} -> loop(Joseph, N, K, Position, Neighbor);
        Other -> error(lists:flatten(io_lib:format("Invalid message: ~p", [Other])))
    end.

loop(Joseph, N, K, Position, Neighbor) ->
    receive
        % When a Hebrew is informed that their neighbor is dead, they receive this message.
        % The NewNeighbor is the next person in the circle after the one who just died.
        % The Hebrew then enters the loop again with this new neighbor.
        {update_neighbor, NewNeighbor} ->
            loop(Joseph, N, K, Position, NewNeighbor);
        % This case is triggered when counter_n reaches N, meaning all but one person (Joseph) is dead.
        % This survivor sends a message back to the main process (Joseph) indicating their position.
        {die, from, _, counter_n, N, counter_k, 1} ->
            Joseph ! {im_joseph, Position};
        % This is the core logic for elimination.
        % When a Hebrew receives this message and counter_k is equal to K, it means it's their turn to die.
        % They notify their previous neighbor (NeighborPrev) to update the neighbor reference and tell their neighbor that it's now their turn to start counting for the next elimination.
        {die, from, NeighborPrev, counter_n, CounterN, counter_k, K} ->
            NeighborPrev ! {update_neighbor, Neighbor},
            Neighbor ! {die, from, NeighborPrev, counter_n, CounterN + 1, counter_k, 1};
        % If counter_k has not yet reached K, the Hebrew passes the message to their neighbor, incrementing counter_k.
        % This process continues until counter_k equals K, at which point the current recipient of the message is eliminated.
        {die, from, _, counter_n, CounterN, counter_k, CounterK} ->
            Neighbor ! {die, from, self(), counter_n, CounterN, counter_k, CounterK + 1},
            loop(Joseph, N, K, Position, Neighbor)
    end.
