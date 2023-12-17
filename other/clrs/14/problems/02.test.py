import unittest

module = __import__('02')


class RodCuttingTest(unittest.TestCase):
    def setUp(self):
        self.prices = [0, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30]

    def test_cutting_without_a_cost(self):
        for lps in [module.lps, module.memoized_lps]:
            self.assertEqual(lps('axbbya'), 'abba')
            self.assertEqual(lps('character'), 'carac')
            self.assertEqual(lps('civic'), 'civic')
            self.assertEqual(lps('xryazcwecaur'), 'racecar')
            self.assertEqual(lps('yaizbohphobiax'), 'aibohphobia')

if __name__ == '__main__':
    unittest.main()
