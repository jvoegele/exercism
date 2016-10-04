-module(allergies).
-export([allergies/1, is_allergic_to/2]).

-type score() :: non_neg_integer().
-type allergen() :: atom().

-define(ALLERGENS, [cats, pollen, chocolate, tomatoes, strawberries, shellfish, peanuts, eggs]).

-spec allergies(score()) -> [allergen()].
allergies(Score) ->
    Result = [Allergen || {Allergen, On} <- lists:zip(?ALLERGENS, score_bits(Score)), On =:= 1],
    %% The test expects the allergies to be returned in specific order, so reverse.
    lists:reverse(Result).

-spec is_allergic_to(allergen(), score()) -> boolean().
is_allergic_to(Item, Score) ->
    lists:member(Item, allergies(Score)).


-spec score_bits(score()) -> list(0|1).
score_bits(Score) ->
    Bits = lists:map(fun digit/1, integer_to_list(Score, 2)),
    ensure_width(Bits, length(?ALLERGENS), 0).

digit($0) ->
    0;
digit($1) ->
    1.

ensure_width(Bits, Width, Pad) ->
    BitsLength = length(Bits),
    if
        BitsLength == Width ->
            Bits;
        BitsLength > Width ->
            lists:nthtail(BitsLength - Width, Bits);
        true ->
            lists:duplicate(Width - BitsLength, Pad) ++ Bits
    end.
