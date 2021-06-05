import unittest

lcs = __import__('02').lcs


class LongestCommonSubsequenceTest(unittest.TestCase):
    def test_lcs(self):
        a = ['A', 'B', 'C', 'B', 'D', 'A', 'B']
        b = ['B', 'D', 'C', 'A', 'B', 'A']

        self.assertEqual(lcs(a, b), ['B', 'C', 'B', 'A'])


if __name__ == '__main__':
    unittest.main()
