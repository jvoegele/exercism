-module(dna).
-export([hamming_distance/2]).

hamming_distance(S1, S2) ->
  length(lists:filter(fun({A, B}) -> A =/= B end, lists:zip(S1, S2))).
