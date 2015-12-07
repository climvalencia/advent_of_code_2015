-module(perfectly_spherical).

-export([
    part1/0,
    part2/0
]).

-export([
    start_db/0,
    stop_db/1
]).

-include_lib("eunit/include/eunit.hrl").

-define(INPUT_FILE, "input").
-define(TAB, gifts_per_house).
-define(NORTH, $^).
-define(SOUTH, $v).
-define(EAST, $<).
-define(WEST, $>).
-define(START, {0,0}).
-define(LEFT, l).
-define(RIGHT, r).

-type coords() :: {integer(), integer()}.
-type db() :: term().

-spec part1() -> integer().
part1() ->
    DB = start_db(),
    InputMap = fetch_map(?INPUT_FILE),
    follow_map(InputMap, DB, ?START),
    Houses = houses_visited(DB),
    stop_db(DB),
    Houses.

part2() ->
    DB = start_db(),
    InputMap = fetch_map(?INPUT_FILE),
    {SantaMap, RobotMap} = split_list(InputMap),
    follow_map(SantaMap, DB, ?START),
    follow_map(RobotMap, DB, ?START),
    Houses = houses_visited(DB),
    stop_db(DB),
    Houses.

-spec start_db() -> db().
start_db() ->
    ets:new(?TAB, [set]).

-spec stop_db(db()) -> true.
stop_db(DB) ->
    ets:delete(DB).


% ==================
% Internal Functions
% ==================
-spec visit(db(), coords()) -> integer().
visit(DB, Coords) ->
    ets:update_counter(DB, Coords, 1, {Coords, 0}).

-spec follow_map(list(), db(), coords()) -> db().
follow_map([], DB, Coords) ->
    visit(DB, Coords),
    DB;
follow_map([H|T], DB, {X,Y}=Coords) ->
    visit(DB, Coords),
    NextCoords = case H of
        ?NORTH -> {X,Y+1};
        ?SOUTH -> {X,Y-1};
        ?EAST -> {X+1,Y};
        ?WEST -> {X-1,Y}
    end,
    follow_map(T, DB, NextCoords).

-spec houses_visited(db()) -> integer().
houses_visited(DB) ->
    ets:info(DB, size).

-spec fetch_map(string()) -> string().
fetch_map(File) ->
    {ok, RawInputBin} = file:read_file(File),
    InputBin = advent_util:chomp(RawInputBin),
    binary_to_list(InputBin).

-spec split_list(list()) -> {list(), list()}.
split_list(List) ->
    split_list(List, [], [], ?LEFT).
split_list([], ListL, ListR, _) ->
    {lists:reverse(ListL), lists:reverse(ListR)};
split_list([H|T], ListL, ListR, ?LEFT) ->
    split_list(T, [H|ListL], ListR, ?RIGHT);
split_list([H|T], ListL, ListR, ?RIGHT) ->
    split_list(T, ListL, [H|ListR], ?LEFT).

% =====
% Tests
% =====
houses_visited_test_() ->
    {foreach,
        fun() -> start_db() end,
        fun(DB) -> stop_db(DB) end,
        [

            fun(DB) ->
                InputMap = ">",
                follow_map(InputMap, DB, ?START),
                ?_assertEqual(2, houses_visited(DB))
            end,

            fun(DB) ->
                InputMap = "^>v<",
                follow_map(InputMap, DB, ?START),
                ?_assertEqual(4, houses_visited(DB))
            end,

            fun(DB) ->
                InputMap = "^v^v^v^v^v",
                follow_map(InputMap, DB, ?START),
                ?_assertEqual(2, houses_visited(DB))
            end
        ]
    }.

robot_santa_test_() ->
    {foreach,
        fun() -> start_db() end,
        fun(DB) -> stop_db(DB) end,
        [
            fun(DB) ->
                InputMap = "^v",
                {SantaMap, RobotMap} = split_list(InputMap),
                follow_map(SantaMap, DB, ?START),
                follow_map(RobotMap, DB, ?START),
                ?_assertEqual(3, houses_visited(DB))
            end,

            fun(DB) ->
                InputMap = "^>v<",
                {SantaMap, RobotMap} = split_list(InputMap),
                follow_map(SantaMap, DB, ?START),
                follow_map(RobotMap, DB, ?START),
                ?_assertEqual(3, houses_visited(DB))
            end,

            fun(DB) ->
                InputMap = "^v^v^v^v^v",
                {SantaMap, RobotMap} = split_list(InputMap),
                follow_map(SantaMap, DB, ?START),
                follow_map(RobotMap, DB, ?START),
                ?_assertEqual(11, houses_visited(DB))
            end
        ]
    }.
