-module(dna).
-export([hamming_distance/2]).

hamming_distance(Strand1, Strand2) ->
    Mutations = [{X, Y} || {X, Y} <- lists:zip(Strand1, Strand2), X =/= Y],
    length(Mutations).
