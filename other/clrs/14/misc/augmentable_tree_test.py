import unittest
from augmentable_tree import AugmentableTree, Color
import random


class Interval:
    def __init__(self, low, high):
        assert low <= high
        self.low = low
        self.high = high

    def __eq__(self, other):
        return isinstance(other, Interval) and self.low == other.low and \
            self.high == other.high

    def __lt__(self, other):
        return self.low < other.low

    def __contains__(self, n):
        return self.low <= n <= self.high

    def __repr__(self):
        return f"Interval({self.low}, {self.high})"

    __str__ = __repr__

    def overlaps(self, other):
        return self.low <= other.high and other.low <= self.high


def max_maybe(*args):
    return max([arg for arg in args if arg is not None])


class IntervalTree(AugmentableTree):
    def augment_node(self, node):
        node.interval = node.key
        node.max = node.interval.high

    def recalculate_node(self, node):
        node.max = max_maybe(
            node.interval.high,
            node.left.max if node.left else None,
            node.right.max if node.right else None
        )

    def find(self, interval):
        node = self.root

        while node:
            if interval.overlaps(node.interval):
                return node
            elif node.left and node.left.max >= interval.low:
                node = node.left
            else:
                node = node.right

        return None


class IntervalTreeTest(unittest.TestCase):
    def test_simple_interval_tree(self):
        tree = IntervalTree()
        three_to_five = Interval(3, 5)
        seven_to_nine = Interval(7, 9)
        eleven_to_thirteen = Interval(11, 13)

        def point(n):
            return Interval(n, n)

        tree.insert(three_to_five)
        tree.insert(seven_to_nine)
        tree.insert(eleven_to_thirteen)

        self.assertIsNone(tree.find(point(1)))
        self.assertIsNone(tree.find(point(2)))

        self.assertIs(tree.find(point(4)).interval, three_to_five)
        self.assertIs(tree.find(point(3)).interval, three_to_five)
        self.assertIs(tree.find(point(5)).interval, three_to_five)

        self.assertIsNone(tree.find(point(6)))

        self.assertIs(tree.find(point(7)).interval, seven_to_nine)
        self.assertIs(tree.find(point(8)).interval, seven_to_nine)
        self.assertIs(tree.find(point(9)).interval, seven_to_nine)

        self.assertIsNone(tree.find(point(10)))

        self.assertIs(tree.find(point(11)).interval, eleven_to_thirteen)
        self.assertIs(tree.find(point(12)).interval, eleven_to_thirteen)
        self.assertIs(tree.find(point(13)).interval, eleven_to_thirteen)

        self.assertIsNone(tree.find(point(14)))
        self.assertIsNone(tree.find(point(15)))

        self.assertProperties(tree)

    def test_overlapping(self):
        tree = IntervalTree()
        tree.insert(Interval(5, 6))
        tree.insert(Interval(1, 20))
        tree.insert(Interval(10, 12))

        self.assertIsNotNone(tree.find(Interval(18, 19)))

        self.assertProperties(tree)

    def test_properties(self):
        k = 20000
        n = 800
        w = 50
        d = 300
        starts = list(range(0, k))
        random.shuffle(starts)
        starts = starts[0:n]
        intervals = []

        tree = IntervalTree()

        for low in starts:
            high = low + random.randint(0, min(w, k - low))
            interval = Interval(low, high)
            intervals.append(interval)
            tree.insert(interval)
            self.assertProperties(tree)

        self.assertProperties(tree)

        random.shuffle(intervals)
        intervals = intervals[0:d]

        for interval in intervals:
            tree.delete(interval)
            self.assertProperties(tree)


    def assertProperties(self, tree):
        def check_max(node):
            numbers = [node.interval.high]

            if node.left:
                numbers.append(check_max(node.left))
                self.assertTrue(node.left.interval.low < node.interval.low)

            if node.right:
                numbers.append(check_max(node.right))
                self.assertTrue(node.interval.low < node.right.interval.low)

            self.assertEqual(node.max, max(numbers))

            return node.max

        if tree.root:
            check_max(tree.root)


def node_size(node):
    return node.size if node else 0

def select_node(node, i):
    while node:
        rank = node_size(node.left) + 1
        if i == rank:
            return node
        elif i < rank:
            node = node.left
        else:
            i -= rank
            node = node.right

    assert(False)

def rank_node(node):
    rank = node_size(node.left) + 1

    while node.parent:
        if node == node.parent.right:
            rank += node_size(node.parent.left) + 1
        node = node.parent

    return rank


class OrderStatisticTree(AugmentableTree):
    def augment_node(self, node):
        node.rank = lambda: rank_node(node)
        node.select = lambda i: select_node(node, i)
        node.size = 1

    def recalculate_node(self, node):
        node.size = 1 + node_size(node.left) + node_size(node.right)

    def select(self, i):
        return select_node(self.root, i)


class OrderStatisticTreeTest(unittest.TestCase):
    def test_rank_when_inserting(self):
        k = 500
        numbers = list(range(1, k + 1))
        random.shuffle(numbers)

        tree = OrderStatisticTree()

        for n in numbers:
            tree.insert(n)

        for n in range(1, k + 1):
            self.assertEqual(tree.select(n).key, n)
            self.assertEqual(tree.search(n).rank(), n)

    def test_rank_when_deleting(self):
        k = 1000
        m = 500

        numbers = list(range(1, k + 1))
        random.shuffle(numbers)

        tree = OrderStatisticTree()

        for n in numbers:
            tree.insert(n)

        random.shuffle(numbers)
        remaining = numbers[0:m]

        for n in numbers[m:]:
            tree.delete(n)

        remaining.sort()

        for (i, n) in enumerate(remaining):
            i += 1
            self.assertEqual(tree.select(i).key, n)
            self.assertEqual(tree.search(n).rank(), i)

    def generate(self, m, n):
        numbers = list(range(m))
        random.shuffle(numbers)
        return numbers[0:n]

    def assertContains(self, tree, numbers):
        for n in numbers:
            self.assertIsNotNone(tree.search(n), f"should contain {n}")
            self.assertEqual(tree.search(n).key, n)

    def assertProperties(self, tree):
        heights = set()
        for n in tree.nodes():
            if not n.left or not n.right:
                heights.add(n.black_height())

            if n.color == Color.RED:
                self.assertEqual(n.left.color, Color.BLACK)
                self.assertEqual(n.right.color, Color.BLACK)

        self.assertEqual(len(heights), 1)
        self.assertEqual(tree.root.color, Color.BLACK)

    def test_insertions(self):
        numbers = self.generate(300, 100)

        tree = OrderStatisticTree()

        for n in numbers:
            tree.insert(n)

        self.assertContains(tree, numbers)

    def test_properties(self):
        numbers = self.generate(300, 100)
        tree = OrderStatisticTree()

        for n in numbers:
            tree.insert(n)

        self.assertProperties(tree)

    def test_deletion(self):
        numbers = self.generate(1000, 500)
        removed = numbers[:]
        random.shuffle(removed)
        removed = removed[0:250]
        remaining = list(set(numbers) - set(removed))

        tree = OrderStatisticTree()

        for n in numbers:
            tree.insert(n)

        for n in removed:
            tree.delete(n)

        self.assertContains(tree, remaining)

        for n in removed:
            self.assertIsNone(tree.search(n))

        self.assertProperties(tree)


if __name__ == '__main__':
    unittest.main()
