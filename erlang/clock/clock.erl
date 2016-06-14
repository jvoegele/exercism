-module(clock).
-export([create/2, to_string/1, is_equal/2, minutes_add/2, minutes_delete/2]).
-export_type([hour/0, minute/0, clock/0]).

-type hour() :: 0..23.
-type minute() :: 0..59.
-opaque clock() :: {hour(), minute()}.

-spec create(hour(), minute()) -> clock().
create(Hour, Minute)
  when Hour >= 0, Hour < 24, Minute >= 0, Minute < 60 ->
    {Hour, Minute}.

-spec to_string(clock()) -> string().
to_string({Hour, Minute}) ->
    string:right(integer_to_list(Hour), 2, $0) ++ ":" ++
    string:right(integer_to_list(Minute), 2, $0).

-spec is_equal(clock(), clock()) -> boolean().
is_equal(Clock1, Clock2) ->
    Clock1 =:= Clock2.

-spec minutes_add(clock(), non_neg_integer()) -> clock().
minutes_add(Clock, Minutes) ->
    apply_minutes_adjustment(Clock, Minutes).

-spec minutes_delete(clock(), non_neg_integer()) -> clock().
minutes_delete(Clock, Minutes) ->
    apply_minutes_adjustment(Clock, -Minutes).

-spec apply_minutes_adjustment(clock(), integer()) -> clock().
apply_minutes_adjustment({Hour, Minute}, Minutes) ->
    NewHour = Hour + (Minutes div 60),
    NewMinute = Minute + (Minutes rem 60),
    roll_over({NewHour, NewMinute}).

-spec roll_over(clock()) -> clock().
roll_over({Hour, Minute}) ->
    {HourAdjustment, NewMinute} = roll_over(Minute, 60),
    {_, NewHour} = roll_over(Hour + HourAdjustment, 24),
    {NewHour, NewMinute}.

-spec roll_over(Value :: integer(), UpperBound :: pos_integer()) ->
    {Adjustment :: -1..1, NewValue :: pos_integer()}.
roll_over(Value, UpperBound) ->
    if Value < 0 ->
           {-1, Value + UpperBound};
       Value >= UpperBound ->
           {+1, Value - UpperBound};
       true ->
           {0, Value}
    end.
