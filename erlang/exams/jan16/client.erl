-module(client).
-export([convert/5]).

% Public functions

convert(from, From, to, To, Temp) when is_number(Temp) andalso From =:= To ->
    io:format("~p°C are equivalent to ~p°C~n", [Temp, Temp]),
    ok;
convert(from, From, to, To, Temp) when is_number(Temp) ->
    Converter = whereis(list_to_atom("From" ++ atom_to_list(From))),
    Converter ! {self(), signal, convert_from, to, To, temp, Temp},
    receive
        {signal, temp_converted, temp, TempConverted} ->
            io:format("~p°~s are equivalent to ~p°~s~n", [Temp, From, TempConverted, To]),
            ok;
        Other ->
            error(io_lib:format("Received unknown response: ~p", [Other]))
    end.
