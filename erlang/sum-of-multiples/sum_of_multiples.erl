-module(sum_of_multiples).
-export([sumOfMultiplesDefault/1, sumOfMultiples/2]).

sumOfMultiplesDefault(N) ->
  sumOfMultiples([3, 5], N).

sumOfMultiples(MultiplesOf, N) ->
  Multiples = [X || X <- lists:seq(0, N - 1),
                    lists:any(fun(E) -> X rem E =:= 0 end, MultiplesOf)],
  lists:foldl(fun(X, Sum) -> X + Sum end, 0, Multiples).
