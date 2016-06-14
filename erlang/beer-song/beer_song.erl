-module(beer_song).
-export([verse/1, sing/2, sing/1]).

verse(N) ->
    capitalize(on_the_wall(N)) ++ ", " ++ bottles_of_beer(N) ++ ".\n" ++
    action(N) ++ ", " ++ on_the_wall(pred(N)) ++ ".\n".

sing(Start) ->
    sing(Start, 0).

sing(Start, Finish) ->
    Verses = [verse(N) || N <- lists:reverse(lists:seq(Finish, Start))],
    string:join(Verses, "\n") ++ "\n".

capitalize([First|Rest]) ->
    [string:to_upper(First)|Rest].

on_the_wall(N) ->
    bottles_of_beer(N) ++ " on the wall".

bottles_of_beer(N) ->
    amount(N) ++ " " ++ bottles(N) ++ " of beer".

action(0) ->
    "Go to the store and buy some more";
action(N) ->
    "Take " ++ pronoun(N) ++ " down and pass it around".

pred(0) ->
    99;
pred(N) ->
    N - 1.

amount(0) ->
    "no more";
amount(N) ->
    integer_to_list(N).

bottles(1) ->
    "bottle";
bottles(_N) ->
    "bottles".

pronoun(1) ->
    "it";
pronoun(_N) ->
    "one".


