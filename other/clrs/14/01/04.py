import functools
import math

@functools.cache
def cut_rod(length, prices):
    if length == 0:
        return 0

    q = prices[length] if length < len(prices) else -math.inf

    for i in range(1, length // 2 + 1):
        q = max(q, cut_rod(i, prices) + cut_rod(length - i, prices))

    return q
