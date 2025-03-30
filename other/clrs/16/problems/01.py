def gray(k):
    n = 0
    for i in range(0, 2**k):
        n ^= (-i) & i
        yield n
