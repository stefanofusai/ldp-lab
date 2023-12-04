-module(client).
-export([convert/5]).

% Public functions

convert(from, From, to, To, Temp) when From =:= To ->
    io:format("~p°C are equivalent to ~p°C~n", [Temp, Temp]),
    ok;
convert(from, From, to, To, Temp) ->
    ConverterFrom = whereis(list_to_atom("From" ++ atom_to_list(From))),
    ConverterTo = whereis(list_to_atom("To" ++ atom_to_list(To))),
    TempConverted = rpc(ConverterFrom, Temp),
    TempFinal = rpc(ConverterTo, TempConverted),
    io:format("~p°~s are equivalent to ~p°~s~n", [Temp, From, TempFinal, To]),
    ok.

% Private functions

rpc(Converter, Temp) ->
    Converter ! {from, self(), temp, Temp},
    receive
        {temp_converted, TempConverted} ->
            TempConverted;
        Other ->
            error(io_lib:format("Received unknown message: ~p", [Other]))
    end.
