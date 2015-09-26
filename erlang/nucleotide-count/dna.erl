-module(dna).
-export([count/2, nucleotide_counts/1]).

%% COUNT ZERO INTERRUPT - on receiving an interrupt, decrement the counter to zero.
-define(COUNT_ZERO, [{"A", 0}, {"T", 0}, {"C", 0}, {"G", 0}]).

count(Dna, Nucleotide) ->
  N = nucleotide(Nucleotide),
  length(lists:filter(fun(X) -> X =:= N end, Dna)).

nucleotide_counts("") ->
  ?COUNT_ZERO;
nucleotide_counts(Dna) ->
  Frequencies = frequencies(Dna, orddict:from_list(?COUNT_ZERO)),
  {Keys, _Counts} = lists:unzip(?COUNT_ZERO),
  %% The tests insist that the counts are in a specific order, so reorder the
  %% Frequencies into the expected order.
  lists:foldl(
    fun(Key, Acc) ->
        Count = orddict:fetch(Key, Frequencies),
        lists:keyreplace(Key, 1, Acc, {Key, Count})
    end,
    ?COUNT_ZERO,
    Keys).

nucleotide(N) ->
  case N of
    "A" -> $A;
    "T" -> $T;
    "C" -> $C;
    "G" -> $G;
    _ -> erlang:error("Invalid nucleotide")
  end.

frequencies(L, Seed) ->
  Fun = fun(X, Acc) ->
            orddict:update_counter([X], 1, Acc)
        end,
  lists:foldl(Fun, Seed, L).
