def lcs_twice(x, y):
    if len(x) < len(y):
        x, y = y, x

    m = len(x)
    n = len(y)

    previous = [0] * (n + 1)

    for i in range(m):
        current = [0] + [None] * n

        for j in range(n):
            if x[i] == y[j]:
                current[j + 1] = previous[j] + 1
            elif previous[j + 1] >= current[j]:
                current[j + 1] = previous[j + 1]
            else:
                current[j + 1] = current[j]

        previous = current

    return previous[n]

def lcs_once_plus_const(x, y):
    if len(x) < len(y):
        x, y = y, x

    m = len(x)
    n = len(y)

    row = [0] * (n + 1)

    for i in range(m):
        prev = 0

        for j in range(n):
            next = row[j + 1]

            if x[i] == y[j]:
                row[j + 1] = prev + 1
            elif row[j + 1] < row[j]:
                row[j + 1] = row[j]

            prev = next

    return row[n]
