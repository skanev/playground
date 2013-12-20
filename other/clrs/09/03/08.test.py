import unittest
import os.path as path

filename = path.join(path.dirname(__file__), '08.py')
exec(open(filename).read())

import random

def shuffled(n):
    items = list(range(n))
    random.shuffle(items)
    return items

class QuantilesTest(unittest.TestCase):
    def test_k_quantiles(self):
        size    = 300
        median  = median_index(size)
        numbers = shuffled(size)
        left, right = sorted(numbers[:size//2]), sorted(numbers[size//2:])

        self.assertEqual(two_array_median(left, right), median)

if __name__ == '__main__':
    unittest.main()
