-module(test).
-export([test/0]).

set() ->
    Var = 1.

print(Var) ->
    Var.

test() ->
    print(Var).

