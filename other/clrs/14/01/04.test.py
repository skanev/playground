import unittest

cut_rod = __import__('04').cut_rod

class RodCuttingTest(unittest.TestCase):
    def setUp(self):
        self.prices = (0, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30)

    def test_cutting_without_a_cost(self):
        self.assertEqual(cut_rod(1, self.prices), 1)
        self.assertEqual(cut_rod(2, self.prices), 5)
        self.assertEqual(cut_rod(3, self.prices), 8)
        self.assertEqual(cut_rod(4, self.prices), 10)
        self.assertEqual(cut_rod(5, self.prices), 13)
        self.assertEqual(cut_rod(6, self.prices), 17)
        self.assertEqual(cut_rod(7, self.prices), 18)
        self.assertEqual(cut_rod(8, self.prices), 22)
        self.assertEqual(cut_rod(9, self.prices), 25)
        self.assertEqual(cut_rod(10, self.prices), 30)
        self.assertEqual(cut_rod(40, self.prices), 120)


if __name__ == '__main__':
    unittest.main()
