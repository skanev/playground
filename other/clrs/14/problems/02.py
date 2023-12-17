import functools

def lps(string):
    chars = list(string)

    memo = [[None] * (len(chars) + 1) for _ in range(len(chars) + 1)]

    for i in range(len(chars)):
        memo[i][i] = ''
        memo[i][i + 1] = chars[i]

    for l in range(2, len(chars) + 1):
        for i in range(len(chars) - l + 1):
            j = i + l

            memo[i][j] = memo[i + 1][j]

            if len(memo[i][j]) < len(memo[i][j - 1]):
                memo[i][j] = memo[i][j - 1]

            if chars[i] == chars[j - 1] and len(memo[i][j]) < len(memo[i + 1][j - 1]) + 2:
                memo[i][j] = chars[i] + memo[i + 1][j - 1] + chars[i]

    return memo[0][len(chars)]

def memoized_lps(string):
    chars = list(string)

    @functools.cache
    def solve(i, j):
        if i == j:
            return ''
        elif i == j - 1:
            return chars[i]

        possible = [
            solve(i + 1, j),
            solve(i, j - 1),
        ]

        if chars[i] == chars[j - 1]:
            possible.append(chars[i] + solve(i + 1, j - 1) + chars[j - 1])

        return max(possible, key=len)

    return solve(0, len(chars))
