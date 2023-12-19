import functools

def bts(points):
    points = [complex(*point) for point in sorted(points, key=lambda p: p[0])]

    best = [
        [0],
        [abs(points[1] - points[0]), abs(points[1] - points[0]) * 2],
    ]

    for j in range(2, len(points)):
        best.append([None] * (j + 1))

        for i in range(0, j - 1):
            best[j][i] = best[j - 1][i] + abs(points[j - 1] - points[j])

        best[j][j - 1] = min(best[j - 1][i] + abs(points[j] - points[i]) for i in range(0, j - 1))
        best[j][j] = best[j][j - 1] + abs(points[j] - points[j - 1])

    return best[-1][-1]
