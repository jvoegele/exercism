-module(grains).
-export([square/1, total/0]).

square(N) ->
    %% round(math:pow(2, N - 1)).
    1 bsl (N - 1).

total() ->
    square(65) - 1.
