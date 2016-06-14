-module(difference_of_squares).
-export([sum_of_squares/1, square_of_sums/1, difference_of_squares/1]).

sum_of_squares(Num) ->
    lists:sum([N * N || N <- lists:seq(1, Num)]).

square_of_sums(Num) ->
    Sum = lists:sum(lists:seq(1, Num)),
    Sum * Sum.

difference_of_squares(Num) ->
    square_of_sums(Num) - sum_of_squares(Num).
