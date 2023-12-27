def knapsack(limit, items):
    t = [[None] * (limit + 1) for _ in range(len(items) + 1)]
    c = [[None] * (limit + 1) for _ in range(len(items) + 1)]

    for i in range(len(items) + 1):
        t[i][0] = 0
        c[i][0] = []

    for j in range(limit + 1):
        t[0][j] = 0
        c[0][j] = []

    for (i, (value, weight)) in enumerate(items):
        for j in range(1, limit + 1):
            if weight > j:
                t[i + 1][j] = t[i][j]
                c[i + 1][j] = c[i][j]
            else:
                w = t[i][j - weight] + value
                if t[i][j] <= t[i][j - weight] + value:
                    t[i + 1][j] = w
                    c[i + 1][j] = c[i][j - weight] + [(value, weight)]
                else:
                    t[i + 1][j] = t[i][j]
                    c[i + 1][j] = c[i][j]

    return t[-1][-1], sum(x[1] for x in c[-1][-1]), c[-1][-1]
