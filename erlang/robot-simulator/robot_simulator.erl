-module(robot_simulator).
-export([create/0, direction/1, position/1, place/3,
         left/1, right/1, advance/1, control/2]).
-export_type([robot/0]).

-type direction() :: north | east | south | west.
-type position() :: {X::integer(), Y::integer()}.
-opaque robot() :: ID::reference().

-spec create() -> robot().
create() ->
    make_ref().

-spec direction(robot()) -> direction() | undefined.
direction(Robot) ->
    {Direction, _Position} = get_robot(Robot),
    Direction.

-spec position(robot()) -> position() | {undefined, undefined}.
position(Robot) ->
    {_Direction, Position} = get_robot(Robot),
    Position.

-spec place(robot(), direction(), position()) -> ok.
place(Robot, Direction, Position) ->
    put(Robot, {Direction, Position}),
    ok.

-spec left(robot()) -> ok.
left(Robot) ->
    {Direction, Position} = get_robot(Robot),
    place(Robot, turn(left, Direction), Position).

-spec right(robot()) -> ok.
right(Robot) ->
    {Direction, Position} = get_robot(Robot),
    place(Robot, turn(right, Direction), Position).

-spec advance(robot()) -> ok.
advance(Robot) ->
    {Direction, Position} = get_robot(Robot),
    place(Robot, Direction, move(Direction, Position)).

-spec control(robot(), string()) -> ok.
control(Robot, Instructions) ->
    lists:foreach(fun(I) ->
                          apply_instruction(Robot, I)
                  end,
                  lists:filter(fun is_valid_instruction/1, Instructions)),
    ok.

get_robot(Robot) ->
    Result = get(Robot),
    case Result of
        undefined ->
            {undefined, {undefined, undefined}};
        {Direction, {X, Y}} ->
            {Direction, {X,  Y}}
    end.

turn(left, north) ->
    west;
turn(left, east) ->
    north;
turn(left, south) ->
    east;
turn(left, west) ->
    south;
turn(right, north) ->
    east;
turn(right, east) ->
    south;
turn(right, south) ->
    west;
turn(right, west) ->
    north.

move(Direction, {X, Y}) ->
    case Direction of
        north ->
            {X, Y+1};
        east ->
            {X+1, Y};
        south ->
            {X, Y-1};
        west ->
            {X-1, Y}
    end.

is_valid_instruction(I) when I =:= $R;
                             I =:= $L;
                             I =:= $A ->
    true;
is_valid_instruction(_) ->
    false.

apply_instruction(Robot, $R) ->
    right(Robot);
apply_instruction(Robot, $L) ->
    left(Robot);
apply_instruction(Robot, $A) ->
    advance(Robot).

