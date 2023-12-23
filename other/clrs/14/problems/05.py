import functools

def edit_distance(x, y, costs):
    @functools.cache
    def distance(i, j):
        if i == len(x):
            return costs['insert'] * (len(y) - j)
        elif j == len(y):
            return min(costs['delete'] * (len(x) - i), costs['kill'])

        possible = []

        if x[i] == y[j]:
            possible.append(distance(i + 1, j + 1) + costs['copy'])

        possible.append(distance(i + 1, j + 1) + costs['replace'])
        possible.append(distance(i + 1, j) + costs['delete'])
        possible.append(distance(i, j + 1) + costs['insert'])

        if i < len(x) - 1 and j < len(y) - 1 and (x[i], x[i + 1]) == (y[j + 1], y[j]):
            possible.append(distance(i + 2, j + 2) + costs['twiddle'])

        return min(possible)

    return distance(0, 0)
