-module(scrabble_score).
-export([test_version/0]).
-export([score/1]).

test_version() ->
  1.

score(Word) ->
    lists:sum(lists:map(fun letter_score/1, string:to_upper(Word))).

letter_score(L) when L == $A;
                     L == $E;
                     L == $I;
                     L == $O;
                     L == $U;
                     L == $L;
                     L == $N;
                     L == $R;
                     L == $S;
                     L == $T ->
    1;
letter_score(L) when L == $D;
                     L == $G ->
    2;
letter_score(L) when L == $B;
                     L == $C;
                     L == $M;
                     L == $P ->
    3;
letter_score(L) when L == $F;
                     L == $H;
                     L == $V;
                     L == $W;
                     L == $Y ->
    4;
letter_score(L) when L == $K ->
    5;
letter_score(L) when L == $J;
                     L == $X ->
    8;
letter_score(L) when L == $Q;
                     L == $Z ->
    10.
