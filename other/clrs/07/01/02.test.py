import unittest
import random
import os.path as path
import random
import time

filename = path.join(path.dirname(__file__), '02.py')
exec(open(filename).read())

class PartitionTest(unittest.TestCase):
    def test_normal_partition(self):
      numbers = [13, 19, 3, 5, 12, 8, 7, 4, 21, 2, 6, 11]
      pivot = partition(numbers)
      self.assertEqual(pivot, 7)
      self.assertEqual(numbers, [3, 5, 8, 7, 4, 2, 6, 11, 21, 13, 19, 12])

    def test_partition_with_repetition(self):
      self.assertEqual(partition([2, 2, 2, 2, 2, 2]), 3)
      self.assertEqual(partition([1, 2, 2, 2, 3, 2]), 3)

    def test_quicksort(self):
      numbers  = [13, 19, 3, 5, 12, 8, 7, 4, 21, 2, 6, 11]
      expected = sorted(numbers)
      quicksort(numbers)
      self.assertEqual(numbers, expected)

if __name__ == '__main__':
    unittest.main()
