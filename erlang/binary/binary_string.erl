-module(binary_string).
-export([to_decimal/1]).

-spec to_decimal(string()) -> non_neg_integer().
to_decimal(String) ->
    lists:sum(apply_position_factors(digits(String))).

-spec digits(string()) -> [0|1].
digits(String) ->
    lists:foldr(fun(D, Acc) ->
                        case D of
                            $0 -> [0|Acc];
                            $1 -> [1|Acc];
                            _ -> Acc
                        end
                end,
                [], String
               ).

%% This seems like a horrible name for a function, but I couldn't
%% think of anything better. The idea is that, starting from the
%% rightmost (least significant) digit in the list, multiply each
%% digit by 2^N, where `N' is the position of that digit in the
%% list counting from the right and starting at 0.
apply_position_factors(Digits) ->
    mult(lists:reverse(Digits), 0, []).

mult([], _, Acc) ->
    Acc;
mult([Digit|Rest], N, Acc) ->
    Pow = round(math:pow(2, N)),
    mult(Rest, N+1, [Digit * Pow|Acc]).

