def select(items, n):
    med     = median(items)
    smaller = [item for item in items if item < med]
    larger  = [item for item in items if item > med]

    if len(smaller) == n:
        return med
    elif len(smaller) > n:
        return select(smaller, n)
    else:
        return select(list(larger), n - len(smaller) - 1)

def median(items):
    def median_index(n):
        if n % 2:
            return n // 2
        else:
            return n // 2 - 1

    def partition(items, element):
        i = 0

        for j in range(len(items) - 1):
            if items[j] == element:
                items[j], items[-1] = items[-1], items[j]

            if items[j] < element:
                items[i], items[j] = items[j], items[i]
                i += 1

        items[i], items[-1] = items[-1], items[i]

        return i

    def select(items, n):
        if len(items) <= 1:
            return items[0]

        medians = []

        for i in range(0, len(items), 5):
            group = sorted(items[i:i + 5])
            items[i:i + 5] = group
            median = group[median_index(len(group))]
            medians.append(median)

        pivot = select(medians, median_index(len(medians)))
        index = partition(items, pivot)

        if n == index:
            return items[index]
        elif n < index:
            return select(items[:index], n)
        else:
            return select(items[index + 1:], n - index - 1)

    return select(items[:], median_index(len(items)))
