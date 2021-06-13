import math

def table(h, w):
    return [[None] * (w + 1) for _ in range(h + 1)]

def optimal(p, q):
    n = len(q) - 1
    e = table(n + 1, n)
    w = table(n + 1, n)
    root = table(n, n)

    for i in range(1, n + 2):
        e[i][i - 1] = q[i - 1]
        w[i][i - 1] = q[i - 1]

    for l in range(1, n + 1):
        for i in range(1, n - l + 2):
            j = i + l - 1
            e[i][j] = math.inf
            w[i][j] = w[i][j - 1] + p[j] + q[j]
            for r in range(i, j + 1):
                t = e[i][r - 1] + e[r + 1][j] + w[i][j]
                if t < e[i][j]:
                    e[i][j] = t
                    root[i][j] = r

    return (e, root)


def dump_tree(root):
    next_d = -1

    def d():
        nonlocal next_d
        next_d += 1
        return f"d{next_d}"

    def is_empty(start, end):
        return start == 0 or end == 0 or start == end + 1

    def collect(start, end):
        split = root[start][end]

        if is_empty(start, split - 1):
            print(f"{d()} is the left child of k{split}")
        else:
            print(f'k{root[start][split - 1]} if left child of k{split}')
            collect(start, split - 1)

        if is_empty(split + 1, end):
            print(f"{d()} is the right child of k{split}")
        else:
            print(f'k{root[split + 1][end]} is right child of k{split}')
            collect(split + 1, end)

    print(f"k{root[1][len(root) - 1]} is the root")

    collect(1, len(root) - 1)

