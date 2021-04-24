def fibonacci(n):
    results = [0] * (n + 1)
    results[1] = 1

    for i in range(2, n + 1):
        results[i] = results[i - 1] + results[i - 2]

    return results[n]
