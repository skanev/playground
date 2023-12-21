import math

def neat_print(words, limit):
    table = [math.inf] * len(words)

    table[0] = (limit - words[0]) ** 3

    for end in range(1, len(words)):
        line_size = -1

        for start in range(end, -1, -1):
            line_size += words[start] + 1

            if line_size > limit: break

            prev = 0 if start == 0 else table[start - 1]
            table[end] = min(table[end], (limit - line_size) ** 3 + prev)

    result = math.inf
    line_size = -1

    for end in range(len(words) - 2, -1, -1):
        line_size += words[end] + 1
        if line_size > limit: break
        result = min(result, table[end])

    return result
