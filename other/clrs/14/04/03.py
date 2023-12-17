def lcs(x, y):
    m = len(x)
    n = len(y)

    cache = {}

    def longest(a, b):
        if a == 0 or b == 0:
            return 0

        if (a, b) in cache:
            return cache[(a, b)]

        result = None

        if x[a - 1] == y[b - 1]:
            result = 1 + longest(a - 1, b - 1)
        else:
            result = max(longest(a - 1, b), longest(a, b - 1))

        cache[(a, b)] = result
        return result

    result = []
    a, b = m, n

    while a != 0 and b != 0:
        if x[a - 1] == y[b - 1]:
            result.append(x[a - 1])
            a -= 1
            b -= 1
        elif longest(a - 1, b) >= longest(a, b - 1):
            a -= 1
        else:
            b -= 1

    result.reverse()

    return result
