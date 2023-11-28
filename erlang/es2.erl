-module(es2).
-export([eval_exp/1]).

eval_op(divv, X, Y) -> X / Y;
eval_op(minus, X, Y) -> X - Y;
eval_op(plus, X, Y) -> X + Y;
eval_op(prod, X, Y) -> X * Y.

eval_exp({Op, {num, X}, {num, Y}}) ->
    eval_op(Op, X, Y);
eval_exp({Op, {num, X}, Exp}) ->
    eval_op(Op, X, eval_exp(Exp));
eval_exp({Op, Exp, {num, Y}}) ->
    eval_op(Op, eval_exp(Exp), Y);
eval_exp({Op, ExpX, ExpY}) ->
    eval_op(Op, eval_exp(ExpX), eval_exp(ExpY)).
