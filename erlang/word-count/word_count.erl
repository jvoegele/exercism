-module(word_count).
-export([count/1]).

count(Text) ->
    counts(words(Text), dict:new()).

words(Text) ->
    [string:to_lower(Word) ||
        Word <- re:split(Text, "[_\\W]", [{return, list}]), Word =/= []
    ].

counts(Words, Dict) ->
    lists:foldl(fun(Word, Acc) ->
                        dict:update_counter(Word, 1, Acc)
                end,
                Dict,
                Words).
