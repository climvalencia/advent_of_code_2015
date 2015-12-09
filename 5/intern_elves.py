import re

INPUT_FILE = 'input'

# PART 1

VOWELS = r'[aeiou]'
REPEATED_CHARACTERS = r'(.)\1+'
FORBIDDEN_STRINGS = ['ab', 'cd', 'pq', 'xy']

def is_nice_part_1(string):
    _, vowel_count = re.subn(VOWELS, '', string)
    has_enough_vowels = vowel_count >= 3
    has_repeated_characters = re.search(REPEATED_CHARACTERS, string) is not None
    has_no_forbidden_strings = True
    for s in FORBIDDEN_STRINGS:
        if s in string:
            has_no_forbidden_strings = False
            break
    # print has_enough_vowels, has_repeated_characters, has_no_forbidden_strings
    return has_enough_vowels and has_repeated_characters and has_no_forbidden_strings

nice_strings = 0
with open(INPUT_FILE) as f:
    for line in f:
        if is_nice_part_1(line):
            nice_strings += 1
print "Part1:", nice_strings


# PART 2

REPEATED_PAIR = r'(..).*\1+'
REPEAT_WITH_IN_BETWEEN = r'(.).\1'

def is_nice_part_2(string):
    has_repeated_pair = re.search(REPEATED_PAIR, string) is not None
    has_repeat_with_in_between = re.search(REPEAT_WITH_IN_BETWEEN, string) is not None
    return has_repeated_pair and has_repeat_with_in_between

nice_strings = 0
with open(INPUT_FILE) as f:
    for line in f:
        if is_nice_part_2(line):
            nice_strings += 1
print "Part2:", nice_strings
