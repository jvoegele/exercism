-module(leap).
-export([leap_year/1]).

leap_year(Year) ->
  Divisible = fun(Divisor) -> (Year rem Divisor) == 0 end,
  Divisible(400) orelse (Divisible(4) andalso not Divisible(100)).
