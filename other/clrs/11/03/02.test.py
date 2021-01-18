import unittest
import os.path as path

from functools import reduce

filename = path.join(path.dirname(__file__), '02.py')
exec(open(filename).read())


def digits_to_number(digits):
    return reduce(lambda a, b: a + b,
                  (d * (K ** i) for (i, d) in enumerate(reversed(digits))))



class HashingTest(unittest.TestCase):
    def test_select(self):
        m = 25
        instances = [
            [127],
            [123, 42],
            [1, 1],
            [12, 31, 85, 12]
        ]
        for instance in instances:
            self.assertEqual(consthash(instance, m),
                             digits_to_number(instance) % m)

if __name__ == '__main__':
    unittest.main()
