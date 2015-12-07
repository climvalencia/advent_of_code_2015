-module(i_was_told).

-export([
    part1/0,
    part2/0
]).

-include_lib("eunit/include/eunit.hrl").

-define(SEP, "x").
-define(INPUT_FILE, "input").

-type dimensions() :: {integer(), integer(), integer()}.


-spec part1() -> integer().
part1() ->
    {ok, IoDevice} = file:open(?INPUT_FILE, [read]),
    Total = get_total_required_paper(IoDevice),
    file:close(IoDevice),
    Total.

-spec part2() -> integer().
part2() ->
    {ok, IoDevice} = file:open(?INPUT_FILE, [read]),
    Total = get_total_required_ribbon(IoDevice),
    file:close(IoDevice),
    Total.

% ==================
% Internal Functions
% ==================
-spec get_total_required_paper(file:io_device()) -> integer().
get_total_required_paper(IoDevice) ->
    get_total_required_paper(IoDevice, 0).

-spec get_total_required_paper(file:io_device(), integer()) -> integer().
get_total_required_paper(IoDevice, SqFeet) ->
    case file:read_line(IoDevice) of
        {ok, RawInput} ->
            Dimensions = parse_input_line(RawInput),
            ThisPresent = surface_area(Dimensions) + slack(Dimensions),
            get_total_required_paper(IoDevice, SqFeet + ThisPresent);
        eof ->
            SqFeet
    end.

-spec surface_area(dimensions()) -> integer().
surface_area({Length, Width, Height}) ->
    2 * (Length*Width + Width*Height + Height*Length).

-spec slack(dimensions()) -> integer().
slack(Dimensions) ->
    area_of_smallest_side(Dimensions).

-spec area_of_smallest_side(dimensions()) -> integer().
area_of_smallest_side(Dimensions) ->
    Sides = tuple_to_list(Dimensions),
    Max = lists:max(Sides),
    [S1, S2] = lists:delete(Max, Sides),
    S1 * S2.

-spec get_total_required_ribbon(file:io_device()) -> integer().
get_total_required_ribbon(IoDevice) ->
    get_total_required_ribbon(IoDevice, 0).

-spec get_total_required_ribbon(file:io_device(), integer()) -> integer().
get_total_required_ribbon(IoDevice, Feet) ->
    case file:read_line(IoDevice) of
        {ok, RawInput} ->
            Dimensions = parse_input_line(RawInput),
            ThisPresent = volume(Dimensions) + smallest_perimeter(Dimensions),
            get_total_required_ribbon(IoDevice, Feet + ThisPresent);
        eof ->
            Feet
    end.

-spec volume(dimensions()) -> integer().
volume({Length, Width, Height}) ->
    Length * Width * Height.

-spec smallest_perimeter(dimensions()) -> integer().
smallest_perimeter(Dimensions) ->
    Sides = tuple_to_list(Dimensions),
    Max = lists:max(Sides),
    [S1, S2] = lists:delete(Max, Sides),
    2 * (S1 + S2).

-spec parse_input_line(string()) -> dimensions().
parse_input_line(RawInput) ->
    Input = advent_util:chomp(RawInput),
    Sides = [ list_to_integer(Side) || Side <- string:tokens(Input, ?SEP) ],
    list_to_tuple(Sides).


% =====
% Tests
% =====
surface_area_test_() ->
    Tests = [{52, {2,3,4}},
             {42, {1,1,10}}],
    [?_assertEqual(SurfaceArea, surface_area(Dimensions)) ||
        {SurfaceArea, Dimensions} <- Tests].

slack_test_() ->
    Tests = [{6, {2,3,4}},
             {1, {1,1,10}}],
    [?_assertEqual(Slack, slack(Dimensions)) ||
        {Slack, Dimensions} <- Tests].

volume_test_() ->
    Tests = [{24, {2,3,4}},
             {10, {1,1,10}}],
    [?_assertEqual(Volume, volume(Dimensions)) ||
        {Volume, Dimensions} <- Tests].

smallest_perimeter_test_() ->
    Tests = [{10, {2,3,4}},
             {4, {1,1,10}}],
    [?_assertEqual(Perimeter, smallest_perimeter(Dimensions)) ||
        {Perimeter, Dimensions} <- Tests].
