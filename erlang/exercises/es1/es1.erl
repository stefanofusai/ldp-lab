-module(es1).
-export([factors/1, is_an_anagram/2, is_palindrome/1, is_proper/1]).

% 1

is_palindrome(String) ->
    StringStripped = lists:filter(
        fun(C) ->
            (C >= $a andalso C =< $z) or (C >= $A andalso C =< $Z)
        end,
        String
    ),
    string:equal(StringStripped, string:reverse(StringStripped)).

% 2

is_an_anagram(String, Dictionary) ->
    lists:any(
        fun(String1) ->
            lists:sort(String) =:= lists:sort(String1)
        end,
        Dictionary
    ).

% 3

is_prime(X) when X =< 3 -> X > 1;
is_prime(X) ->
    lists:all(
        fun(N) -> X rem N /= 0 end,
        lists:seq(2, trunc(math:sqrt(X)))
    ).

factors(X) ->
    [Y || Y <- lists:seq(2, X), is_prime(Y)].

% 4

sum(Acc, []) ->
    Acc;
sum(Acc, [H | T]) ->
    sum(Acc + H, T).
sum([]) ->
    0;
sum([H | T]) ->
    sum(H, T).

is_proper(X) ->
    Divisors = [Y || Y <- lists:seq(1, X - 1), X rem Y == 0],
    X == sum(Divisors).
