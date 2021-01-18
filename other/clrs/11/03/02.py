K = 128


def consthash(digits, m):
    result = 0
    power = 1

    for d in reversed(digits):
        result += ((d % m) * power) % m
        result %= m
        power = (power * K) % m

    return result
