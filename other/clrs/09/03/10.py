def two_array_median(a, b):
    if len(a) == 1:
        return min(a[0], b[0])

    m = median_index(len(a))
    i = m + 1
    if a[m] < b[m]:
        return two_array_median(a[-i:], b[:i])
    else:
        return two_array_median(a[:i], b[-i:])

def median_index(n):
    if n % 2:
        return n // 2
    else:
        return n // 2 - 1
