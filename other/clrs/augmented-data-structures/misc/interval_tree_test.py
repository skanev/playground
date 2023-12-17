import unittest
from interval_tree import IntervalTree, Color, Interval
import random


class IntervalTest(unittest.TestCase):
    def test_contains(self):
        interval = Interval(5, 10)
        self.assertTrue(7 in interval)
        self.assertTrue(5 in interval)
        self.assertTrue(10 in interval)
        self.assertTrue(4 not in interval)
        self.assertTrue(11 not in interval)

    def test_overlaps(self):
        self.assertTrue(Interval(5, 10).overlaps(Interval(2, 7)))
        self.assertTrue(Interval(5, 10).overlaps(Interval(7, 9)))
        self.assertTrue(Interval(5, 10).overlaps(Interval(7, 13)))

        self.assertFalse(Interval(5, 10).overlaps(Interval(1, 4)))
        self.assertFalse(Interval(5, 10).overlaps(Interval(11, 15)))


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

        self.assertIsNone(tree.search(point(1)))
        self.assertIsNone(tree.search(point(2)))

        self.assertIs(tree.search(point(4)).interval, three_to_five)
        self.assertIs(tree.search(point(3)).interval, three_to_five)
        self.assertIs(tree.search(point(5)).interval, three_to_five)

        self.assertIsNone(tree.search(point(6)))

        self.assertIs(tree.search(point(7)).interval, seven_to_nine)
        self.assertIs(tree.search(point(8)).interval, seven_to_nine)
        self.assertIs(tree.search(point(9)).interval, seven_to_nine)

        self.assertIsNone(tree.search(point(10)))

        self.assertIs(tree.search(point(11)).interval, eleven_to_thirteen)
        self.assertIs(tree.search(point(12)).interval, eleven_to_thirteen)
        self.assertIs(tree.search(point(13)).interval, eleven_to_thirteen)

        self.assertIsNone(tree.search(point(14)))
        self.assertIsNone(tree.search(point(15)))

        self.assertProperties(tree)

    def test_overlapping(self):
        tree = IntervalTree()
        tree.insert(Interval(5, 6))
        tree.insert(Interval(1, 20))
        tree.insert(Interval(10, 12))

        self.assertIsNotNone(tree.search(Interval(18, 19)))

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


if __name__ == '__main__':
    unittest.main()
