def mono(items):
    best = [1] * len(items)
    choices = [None] * len(items)

    for j in range(1, len(items)):
        for i in range(0, j):
            if items[i] <= items[j] and best[j] <= best[i] + 1:
                best[j] = best[i] + 1
                choices[j] = i

    n = max(range(0, len(items)), key=lambda x: (best[x], x))

    result = []

    while n is not None:
        result.append(items[n])
        n = choices[n]

    result.reverse()

    return result
