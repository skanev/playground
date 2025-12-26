import unittest
import random
from collections import defaultdict


josephus = __import__('02').josephus


def naive_josephus(n, m):
    items = list(range(1, n + 1))
    current = 0
    result = []

    while items:
        current = (current + m - 1) % len(items)
        result.append(items[current])
        del items[current]

    return tuple(result)


class JosephusTest(unittest.TestCase):
    def test_simple_permutation(self):
        self.assertEqual(josephus(7, 3), (3, 6, 2, 7, 5, 1, 4))

    def test_permutations(self):
        n = 50
        for m in range(1, n):
            self.assertEqual(josephus(n, m), naive_josephus(n, m))


if __name__ == '__main__':
    unittest.main()
