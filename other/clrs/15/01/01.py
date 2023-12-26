import functools


def top_down(activities):
    sentinel = max(a[1] for a in activities)
    activities = [
        (-1, -1),
        *sorted(activities, key=lambda x: x[1]),
        (sentinel, sentinel),
    ]

    @functools.cache
    def cost(i, j):
        if i >= j:
            return 0

        return max(
            (
                cost(i, k) + cost(k, j) + 1
                for k in range(i + 1, j)
                if activities[i][1] <= activities[k][0]
                and activities[k][1] <= activities[j][0]
            ),
            default=0,
        )

    return cost(0, len(activities) - 1)


def bottom_down(activities):
    sentinel = max(a[1] for a in activities)
    act = [(-1, -1), *sorted(activities, key=lambda x: x[1]), (sentinel, sentinel)]
    c = [[None] * len(act) for _ in range(len(act))]

    for i in range(0, len(act)):
        c[i][i] = 0

    for span in range(1, len(act)):
        for i in range(0, len(act) - span):
            j = i + span
            q = 0
            for k in range(i + 1, j):
                if act[i][1] <= act[k][0] and act[k][1] <= act[j][0]:
                    q = max(q, c[i][k] + c[k][j] + 1)
            c[i][j] = q

    return c[0][-1]
