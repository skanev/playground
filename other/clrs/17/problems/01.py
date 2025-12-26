import sys, os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'misc'))

from augmentable_tree import AugmentableTree

class Endpoint:
    def __init__(self, value, weight): self.value, self.weight = value, weight
    def __lt__(self, other): return (self.value, -self.weight) < (other.value, -other.weight)
    def __eq__(self, other): return (self.value, self.weight) == (other.value, other.weight)
    def isLow(self): return self.weight == 1


def weight(node):
    return node.weight if node else 0


def optimal(node):
    return node.optimal if node else (0, None)


class OverlapTree(AugmentableTree):
    def __init__(self, intervals = []):
        super(OverlapTree, self).__init__()

        for interval in intervals:
            self.insert_interval(interval)

    def augment_node(self, node):
        node.optimal = (1, node.key.value) if node.key.isLow() else (0, None)
        node.weight = node.key.weight
        node.sexp = lambda: self.print_node(node)

    def recalculate_node(self, node):
        node.weight = node.key.weight + weight(node.left) + weight(node.right)

        right_optimal = optimal(node.right)

        candidates = [
            optimal(node.left),
            (weight(node.left) + node.key.weight, node.key.value),
            (weight(node.left) + node.key.weight + right_optimal[0], right_optimal[1] or node.key.value),
        ]

        node.optimal = max(candidates, key=lambda t: t[0])

    def max_overlap(self):
        return optimal(self.root)[1]

    def insert_interval(self, interval):
        self.insert(Endpoint(interval.low, 1))
        self.insert(Endpoint(interval.high, -1))

    def delete_interval(self, interval):
        self.delete(Endpoint(interval.low, 1))
        self.delete(Endpoint(interval.high, -1))

