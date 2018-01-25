-module(all_your_base).
-export([test_version/0]).
-export([convert/3]).

test_version() ->
  1.

convert(Digits, FromBase, ToBase) ->
    try validate(Digits, FromBase, ToBase) of
        ok ->
            {ok, Digits}
    catch
        throw:Error ->
            {error, Error}
    end.

validate(Digits, FromBase, ToBase) ->
    validate_bases(FromBase, ToBase),
    validate_no_negative_digits(Digits),
    validate_digits_in_base(Digits, FromBase),
    ok.

validate_bases(FromBase, _ToBase) when FromBase < 2 ->
    throw(invalid_src_base);
validate_bases(_FromBase, ToBase) when ToBase < 2 ->
    throw(invalid_dst_base);
validate_bases(_FromBase, _ToBase) ->
    ok.

validate_no_negative_digits(Digits) ->
    case lists:any(fun(D) -> D < 0 end, Digits) of
        true ->
            throw(negative);
        false ->
            ok
    end.

validate_digits_in_base(Digits, Base) ->
    case lists:any(fun(D) -> D >= Base end, Digits) of
        true ->
            throw(not_in_base);
        false ->
            ok
    end.
