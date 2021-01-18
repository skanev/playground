from math import floor

m = 1000
s = 2654435769
A = s / (2 ** 32)


def h(k):
    ka = k * A
    frac = ka - floor(ka)

    return int(m * frac)
