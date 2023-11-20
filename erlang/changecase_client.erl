-module(changecase_client).
-export([changecase/3]).

changecase(Server, StrIn, Command) ->
    Server ! {self(), {StrIn, Command}},
    receive
        {Server, StrOut} -> StrOut
    end.