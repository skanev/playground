import unittest

cut_rod = __import__('05').memoized_cut_rod


class MemoizedRodCuttingTest(unittest.TestCase):
    def setUp(self):
        self.prices = [0, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30]

    def test_cutting_without_a_cost(self):
        self.assertEqual(cut_rod(1, self.prices), (1, [1]))
        self.assertEqual(cut_rod(2, self.prices), (5, [2]))
        self.assertEqual(cut_rod(3, self.prices), (8, [3]))
        self.assertEqual(cut_rod(4, self.prices), (10, [2, 2]))
        self.assertEqual(cut_rod(5, self.prices), (13, [2, 3]))
        self.assertEqual(cut_rod(6, self.prices), (17, [6]))
        self.assertEqual(cut_rod(7, self.prices), (18, [1, 6]))
        self.assertEqual(cut_rod(8, self.prices), (22, [2, 6]))
        self.assertEqual(cut_rod(9, self.prices), (25, [3, 6]))
        self.assertEqual(cut_rod(10, self.prices), (30, [10]))
        self.assertEqual(cut_rod(40, self.prices), (120, [10, 10, 10, 10]))


if __name__ == '__main__':
    unittest.main()
