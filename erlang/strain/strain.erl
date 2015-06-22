-module(strain).
-export([keep/2, discard/2]).

keep(P, L) ->
  [E || E <- L, P(E)].

discard(P, L) ->
  keep(fun(E) -> not P(E) end, L).
