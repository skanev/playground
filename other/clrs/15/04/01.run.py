solution = __import__('01')

a = [1, 0, 0, 1, 0, 1, 0, 1]
b = [0, 1, 0, 1, 1, 1, 0, 1, 1, 0]

print(solution.lcs(a, b))
