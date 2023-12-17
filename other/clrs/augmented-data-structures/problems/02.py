import sys, os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'misc'))

from order_statistic_tree import OrderStatisticTree

def josephus(n, m):
    tree = OrderStatisticTree()

    for i in list(range(1, n + 1)):
        tree.insert(i)

    current = 1
    result = []

    while tree.root:
        current = (current + m - 2) % tree.root.size + 1
        node = tree.select(current)
        result.append(node.key)
        tree.delete(node.key)

    return tuple(result)


