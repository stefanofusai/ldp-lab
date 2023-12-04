-module(tempsys).
-export([startsys/0]).

% Public functions

startsys() ->
    Converters = {
        {
            from, [
                {symbol, 'C', convert_fun, fun from_C/1},
                {symbol, 'De', convert_fun, fun from_De/1},
                {symbol, 'F', convert_fun, fun from_F/1},
                {symbol, 'K', convert_fun, fun from_K/1},
                {symbol, 'N', convert_fun, fun from_N/1},
                {symbol, 'R', convert_fun, fun from_R/1},
                {symbol, 'Re', convert_fun, fun from_Re/1},
                {symbol, 'Ro', convert_fun, fun from_Ro/1}
            ]
        },
        {
            to, [
                {symbol, 'C', convert_fun, fun to_C/1},
                {symbol, 'De', convert_fun, fun to_De/1},
                {symbol, 'F', convert_fun, fun to_F/1},
                {symbol, 'K', convert_fun, fun to_K/1},
                {symbol, 'N', convert_fun, fun to_N/1},
                {symbol, 'R', convert_fun, fun to_R/1},
                {symbol, 'Re', convert_fun, fun to_Re/1},
                {symbol, 'Ro', convert_fun, fun to_Ro/1}
            ]
        }
    },
    {{_, ConvertersFrom}, {_, ConvertersTo}} = Converters,
    lists:foreach(
        fun({symbol, Symbol, convert_fun, ConvertFun}) ->
            register(
                list_to_atom("From" ++ atom_to_list(Symbol)),
                spawn(fun() -> loop(ConvertFun) end)
            )
        end,
        ConvertersFrom
    ),
    lists:foreach(
        fun({symbol, Symbol, convert_fun, ConvertFun}) ->
            register(
                list_to_atom("To" ++ atom_to_list(Symbol)),
                spawn(fun() -> loop(ConvertFun) end)
            )
        end,
        ConvertersTo
    ),
    ok.

% Private functions

from_C(T) -> T.
from_De(T) -> 100 - T * 2 / 3.
from_F(T) -> (T - 32) * 5 / 9.
from_K(T) -> T - 273.15.
from_N(T) -> T * 100 / 33.
from_R(T) -> (T - 491.67) * 5 / 9.
from_Re(T) -> T * 5 / 4.
from_Ro(T) -> (T - 7.5) * 40 / 21.

to_C(T) -> T.
to_De(T) -> (100 - T) * 3 / 2.
to_F(T) -> T * 9 / 5 + 32.
to_K(T) -> T + 273.15.
to_N(T) -> T * 33 / 100.
to_R(T) -> (T + 273.15) * 9 / 5.
to_Re(T) -> T * 4 / 5.
to_Ro(T) -> T * 21 / 40 + 7.5.

loop(ConvertFun) ->
    receive
        {from, From, temp, Temp} when is_number(Temp) ->
            From ! {temp_converted, ConvertFun(Temp)};
        Other ->
            io:format("Received unknown message: ~p~n", [Other])
    end,
    loop(ConvertFun).
