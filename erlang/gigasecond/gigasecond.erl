-module(gigasecond).
-export([from/1]).

-define(BILLION, 1000000000).

from(Date = {_Y, _M, _D}) ->
    from({Date, {0, 0, 0}});
from({Date, Time}) ->
    Seconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
    calendar:gregorian_seconds_to_datetime(Seconds + ?BILLION).
