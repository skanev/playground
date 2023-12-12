import unittest
import os.path as path

filename = path.join(path.dirname(__file__), '06.py')
exec(open(filename).read())

import random

def shuffled(n):
    items = list(range(n))
    random.shuffle(items)
    return items

def visualize(n, k):
    items = shuffled(n)
    quantiles = k_quantiles(items, k)
    result = '-' * len(items)

    for q in quantiles:
        result = result[:q] + 'o' + result[q+1:]

    return result

class QuantilesTest(unittest.TestCase):
    def test_k_quantiles(self):
        self.assertEqual(k_quantiles(shuffled(11), 4), [2, 5, 8])
        self.assertEqual(k_quantiles(shuffled(14), 5), [2, 5, 8, 11])
        self.assertEqual(k_quantiles(shuffled(17), 6), [2, 5, 8, 11, 14])
        self.assertEqual(k_quantiles(shuffled(20), 7), [2, 5, 8, 11, 14, 17])
        self.assertEqual(k_quantiles(shuffled(23), 8), [2, 5, 8, 11, 14, 17, 20])

    def test_visualized_quantiles(self):
        self.assertEqual(visualize(3, 1),  '---')
        self.assertEqual(visualize(3, 2),  '-o-')
        self.assertEqual(visualize(5, 2),  '--o--')
        self.assertEqual(visualize(2, 3),  'oo')
        self.assertEqual(visualize(5, 3),  '-o-o-')
        self.assertEqual(visualize(8, 3),  '--o--o--')
        self.assertEqual(visualize(11, 3), '---o---o---')
        self.assertEqual(visualize(11, 4), '--o--o--o--')
        self.assertEqual(visualize(9, 5),  '-o-o-o-o-')
        self.assertEqual(visualize(14, 5), '--o--o--o--o--')

if __name__ == '__main__':
    unittest.main()
