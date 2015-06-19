-module(bob).
-export([response_for/1]).

response_for(S) ->
  case {is_question(S), is_shouting(S), is_silence(S)} of
    {_, _, true} -> "Fine. Be that way!";
    {_, true, _} -> "Whoa, chill out!";
    {true, _, _} -> "Sure.";
    _ -> "Whatever."
  end.

is_question(S) -> 
  case re:run(S, "\\?$") of
    {match, _} -> true;
    nomatch -> false
  end.

is_shouting(S) ->
  AtLeastOneUpperCase = "\\p{Lu}",
  case re:run(S, AtLeastOneUpperCase) of
    {match, _} -> S =:= string:to_upper(S);
    nomatch -> false
  end.

is_silence(S) -> [] =:= string:strip(S).
