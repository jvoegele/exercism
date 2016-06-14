-module(etl).
-export([transform/1]).

-type old_format() :: [{pos_integer(), [string()]}].
-type new_format() :: [{string(), pos_integer()}].

-spec transform(old_format()) -> new_format().
transform(Old) ->
    lists:flatmap(fun transform_entry/1, Old).

transform_entry({Score, Letters}) ->
    [{string:to_lower(Letter), Score} || Letter <- Letters].
