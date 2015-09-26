-module(phone).
-export([number/1, areacode/1, pretty_print/1]).

number(Str) ->
  if_valid(Str, fun(Digits) ->
                    Digits
                end).

areacode(Str) ->
  if_valid(Str, fun(Digits) ->
                    string:substr(Digits, 1, 3)
                end).

pretty_print(Str) ->
  Format = fun(Digits) ->
               Area = string:substr(Digits, 1, 3),
               Exchange = string:substr(Digits, 4, 3),
               Subscriber = string:substr(Digits, 7),
               "(" ++ Area ++ ") " ++ Exchange ++ "-" ++ Subscriber
           end,
  if_valid(Str, Format).

if_valid(Str, Fun) ->
  case digits(Str) of
    false -> "0000000000";
    Digits -> Fun(Digits)
  end.

digits(Str) ->
  Digits = [D || D <- Str, D >= $0, D =< $9],
  valid_digits(Digits).

valid_digits(Str) when length(Str) < 10 ->
  false;
valid_digits(Str) when length(Str) == 10 ->
  Str;
valid_digits(Str) when length(Str) == 11 ->
  case string:substr(Str, 1, 1) of
    "1" -> string:substr(Str, 2);
    _ -> false
  end;
valid_digits(Str) when length(Str) > 11 ->
  false.
