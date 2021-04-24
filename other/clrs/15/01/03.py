def cut_rod(length, prices, cut_cost=0):
    values = [-1] * (length + 1)
    choices = [-1] * (length + 1)
    values[0] = 0

    for j in range(1, length + 1):
        max_cut = min(len(prices), j + 1)
        max_value = -1
        for i in range(1, max_cut):
            value = prices[i] + values[j - i] - (0 if j - i == 0 else cut_cost)
            if max_value < value:
                max_value = value
                choices[j] = i

        values[j] = max_value

    n = length
    cuts = []
    while n > 0:
        cuts.append(choices[n])
        n -= choices[n]

    return (values[length], cuts)
