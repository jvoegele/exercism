-module(triangle).
-export([test_version/0]).
-export([kind/3]).

test_version() ->
  1.

kind(A, B, C) when A =< 0; B =< 0; C =< 0 ->
    {error, "all side lengths must be positive"};
kind(A, B, C) when A + B =< C; A + C =< B; B + C =< A ->
    {error, "side lengths violate triangle inequality"};
kind(A, A, A) ->
    equilateral;
kind(A, B, C) when A == B; A == C; B == C ->
    isosceles;
kind(_A, _B, _C) ->
    scalene.
