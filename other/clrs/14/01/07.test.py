import unittest
import os.path as path
import random

filename = path.join(path.dirname(__file__), '07.py')
exec(open(filename).read())


def count_inversions(numbers):
    count = 0
    for i in range(0, len(numbers)):
        for j in range(i + 1, len(numbers)):
            if numbers[i] > numbers[j]:
                count += 1
    return count


class OrderStatisticTreeTest(unittest.TestCase):
    def test_inversions(self):
        numbers = [n * 2 for n in range(0, 100)]
        random.shuffle(numbers)
        self.assertEqual(inversions(numbers), count_inversions(numbers))


if __name__ == '__main__':
    unittest.main()
