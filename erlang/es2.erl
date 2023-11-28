-module(es2).
-export([eval_exp/1]).

eval_op(divv) -> fun(X, Y) -> X / Y end;
eval_op(minus) -> fun(X, Y) -> X - Y end;
eval_op(plus) -> fun(X, Y) -> X + Y end;
eval_op(prod) -> fun(X, Y) -> X * Y end.

exec_exp(Op, X, Y) ->
    Fun = eval_op(Op),
    Fun(X, Y).

eval_exp({Op, {num, X}, {num, Y}}) ->
    exec_exp(Op, X, Y);
eval_exp({Op, {num, X}, Exp}) ->
    exec_exp(Op, X, eval_exp(Exp));
eval_exp({Op, Exp, {num, Y}}) ->
    exec_exp(Op, eval_exp(Exp), Y);
eval_exp({Op, ExpX, ExpY}) ->
    exec_exp(Op, eval_exp(ExpX), eval_exp(ExpY)).
