def memoized_cut_rod(length, prices):
    values = [-1] * (length + 1)
    choices = [-1] * (length + 1)

    def cut(n):
        if values[n] >= 0:
            return values[n]

        if n == 0:
            values[0] = 0
        else:
            cut_options = range(1, min(len(prices), n + 1))
            max_value = -1
            for i in cut_options:
                value = prices[i] + cut(n - i)

                if max_value < value:
                    max_value = value
                    choices[n] = i

            values[n] = max_value

        return values[n]

    value = cut(length)
    cuts = []

    while length > 0:
        cuts.append(choices[length])
        length -= choices[length]

    return (value, cuts)
