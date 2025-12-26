import unittest
import random
from collections import defaultdict

OverlapTree = __import__('01').OverlapTree

class Interval:
    def __init__(self, low, high):
        self.low = low
        self.high = high

    def elements(self):
        return range(self.low, self.high + 1)


def naive_max_overlap(intervals):
    counts = defaultdict(lambda: 0)

    for interval in intervals:
        for number in interval.elements():
            counts[number] += 1

    count = max(counts.values())
    answer = min(key for key, value in counts.items() if value == count)

    return answer

class OverlapTreeTest(unittest.TestCase):
    def test_simple_case(self):
        intervals = [
            Interval(0, 6),
            Interval(3, 7),
            Interval(4, 10)
        ]

        tree = OverlapTree(intervals)

        self.assertEqual(naive_max_overlap(intervals), 4)
        self.assertEqual(tree.max_overlap(), 4)

    def test_random_case(self):
        n = 100
        numbers = list(range(0, n * 2))
        random.shuffle(numbers)
        intervals = []

        tree = OverlapTree()

        while numbers:
            low, high = numbers.pop(), numbers.pop()
            if low > high:
                low, high = high, low

            interval = Interval(low, high)
            intervals.append(interval)
            tree.insert_interval(interval)

        while intervals:
            endpoint = naive_max_overlap(intervals)
            self.assertEqual(tree.max_overlap(), endpoint)

            interval, = [i for i in intervals if i.low == endpoint]
            intervals.remove(interval)
            tree.delete_interval(interval)

if __name__ == '__main__':
    unittest.main()
