import math

def break_string(breaks, n):
    breaks = (0, *sorted(breaks), n)
    best = [[0] * len(breaks) for _ in range(len(breaks))]

    for width in range(2, len(breaks)):
        for i in range(len(breaks) - width):
            j, q = i + width, math.inf
            for k in range(i + 1, j):
                q = min(q, best[i][k] + best[k][j] + breaks[j] - breaks[i])
            best[i][j] = q

    return best[0][len(breaks) - 1]
