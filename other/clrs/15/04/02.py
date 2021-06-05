def lcs(x, y):
    m = len(x)
    n = len(y)

    longest = [[0] * (n + 1) for _ in range(m + 1)]

    for i in range(m):
        for j in range(n):
            if x[i] == y[j]:
                longest[i + 1][j + 1] = longest[i][j] + 1
            elif longest[i][j + 1] >= longest[i + 1][j]:
                longest[i + 1][j + 1] = longest[i][j + 1]
            else:
                longest[i + 1][j + 1] = longest[i + 1][j]

    result = []
    a, b = m, n

    while a != 0 and b != 0:
        if longest[a - 1][b - 1] >= longest[a - 1][b] and longest[a - 1][b - 1] >= longest[a][b - 1]:
            result.append(x[a - 1])
            a -= 1
            b -= 1
        elif longest[a - 1][b] >= longest[a][b - 1]:
            a -= 1
        else:
            b -= 1

    result.reverse()

    return result
