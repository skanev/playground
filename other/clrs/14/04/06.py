def mono(items):
    last = 0
    longest = [None] * len(items)
    longest[0] = dict(value=items[0], prev=None)

    def find_place(value):
        lo, hi = 0, last + 1

        while lo < hi:
            mid = (lo + hi) // 2

            if longest[mid]['value'] <= value:
                lo = mid + 1
            else:
                hi = mid

        return (lo, longest[lo - 1] if lo > 0 else None)

    for i in range(1, len(items)):
        (index, prev) = find_place(items[i])
        longest[index] = dict(value=items[i], prev=prev)
        last = max(last, index)

    result = []
    node = longest[last]

    while node:
        result.append(node['value'])
        node = node['prev']

    result.reverse()

    return result
