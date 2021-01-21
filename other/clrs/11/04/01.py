def populate(m, keys, probe):
    table = [None] * m

    for key in keys:
        i = 0
        for _ in range(m):
            pos = probe(key, i)
            i += 1

            if table[pos] is None:
                table[pos] = key
                break
        else:
            raise RuntimeError(f"Could not put element {key} in {table!r}")

    return table


def linear(m):
    return lambda key, i: (key + i) % m


def quadratic(m, c1, c2):
    return lambda key, i: (key + i * c1 + i * c2 * c2) % m


def double(m):
    return lambda key, i: (key + i * (1 + key % (m - 1))) % m
