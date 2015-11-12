-module(grade_school).
-export([new/0, add/3, get/2, sort/1]).
-export_type([school/0]).

-type grade() :: 1..7.
-type name() :: string().
-opaque school() :: #{grade() => [name()]}.

-spec new() -> school().
new() ->
    #{}.

-spec add(name(), grade(), school()) -> school().
add(Name, Grade, School) ->
    School#{Grade => [Name|get(Grade, School)]}.

-spec get(grade(), school() | []) -> [name()].
%% We need this clause because the test suite passes an empty list
%% for the School argument in one test case.
get(_Grade, []) ->
    new();
get(Grade, School) ->
    maps:get(Grade, School, []).

-spec sort(school()) -> [{grade(), [name()]}].
sort(School) ->
    Fun = fun(K, V, Acc) ->
                  [{K, lists:sort(V)}|Acc]
          end,
    lists:reverse(maps:fold(Fun, [], School)).
