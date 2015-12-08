import md5

SECRET_KEY = 'bgvyzdsv'
BIG_NUMBER = pow(2,32)

def find_number_with_prefix(prefix):
    for x in xrange(1, BIG_NUMBER):
        hash_input = SECRET_KEY + str(x)
        hash_output = (md5.md5(hash_input)).hexdigest()
        if hash_output[0:len(prefix)] == prefix:
            return x


print "Part1:", find_number_with_prefix('00000')
print "Part2:", find_number_with_prefix('000000')
