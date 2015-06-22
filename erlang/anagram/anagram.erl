-module(anagram).
-export([find/2]).

find(Word, Candidates) ->
  lists:filter(fun(C) -> is_anagram(Word, C) end, Candidates).

is_anagram(Word, Candidate) ->
  W = string:to_lower(Word),
  C = string:to_lower(Candidate),
  W =/= C andalso lists:sort(W) =:= lists:sort(C).
