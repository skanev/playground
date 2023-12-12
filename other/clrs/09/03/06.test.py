import unittest
import os.path as path

filename = path.join(path.dirname(__file__), '05.py')
exec(open(filename).read())

import random

class SelectionTest(unittest.TestCase):
    def test_select(self):
        items = list(range(1, 10000))
        random.shuffle(items)
        self.assertEqual(select(items[:], 42), 43)
        self.assertEqual(select(items[:], 5012), 5013)
        self.assertEqual(select(items[:], 9998), 9999)

if __name__ == '__main__':
    unittest.main()
