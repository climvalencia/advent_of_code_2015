INPUT_FILE = 'input'
FIRST = 0
ACTIONS = ['toggle', 'on', 'off']
IS_OFF = False

class ParsingError(Exception):
    pass


# PARSING
def parse_instructions(line):
    keywords = line.split(' ')
    action = extract_action(keywords)
    from_coords = extract_coords(keywords)
    expect_through(keywords)
    to_coords = extract_coords(keywords)
    return (action, from_coords, to_coords)

def extract_action(keywords):
    action = keywords.pop(FIRST)
    if action == 'turn':
        action = keywords.pop(FIRST)
    if action not in ACTIONS:
        raise ParsingError
    return action

def extract_coords(keywords):
    coords_str = keywords.pop(FIRST)
    [x,y] = [int(num_str) for num_str in coords_str.split(',')]
    return (x, y)

def expect_through(keywords):
    through = keywords.pop(FIRST)
    if not (through == 'through'):
        raise ParsingError

# PART 1
def flip_switches(lights, instructions):
    (action, (x1, y1), (x2, y2)) = instructions
    lights_x = x2 - x1
    lights_y = y2 - y1
    for light_x in xrange(x1, x2+1):
        for light_y in xrange(y1, y2+1):
            current_light = (light_x, light_y)
            if action == 'toggle':
                try:
                    lights.remove(current_light)
                except KeyError:
                    lights.add(current_light)
            elif action == 'on':
                lights.add(current_light)
            elif action == 'off':
                lights.discard(current_light)

lights = set()
with open(INPUT_FILE) as f:
    for line in f:
        instructions = parse_instructions(line)
        flip_switches(lights, instructions)
print "Part1:", len(lights)


# PART 2
def adjust_brightness(lights, instructions):
    (action, (x1, y1), (x2, y2)) = instructions
    lights_x = x2 - x1
    lights_y = y2 - y1
    for light_x in xrange(x1, x2+1):
        for light_y in xrange(y1, y2+1):
            current_light = (light_x, light_y)
            brightness = lights.pop(current_light, IS_OFF)
            if action == 'toggle':
                brightness += 2
            elif action == 'on':
                brightness += 1
            elif action == 'off':
                brightness += -1
            if brightness > 0:
                lights[current_light] = brightness

lights = {}
with open(INPUT_FILE) as f:
    for line in f:
        instructions = parse_instructions(line)
        adjust_brightness(lights, instructions)
print "Part2:", sum(lights.values())
