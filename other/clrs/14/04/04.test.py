import unittest

lcs = __import__('04')


class LongestCommonSubsequenceTest(unittest.TestCase):
    def test_example_one(self):
        a = ['A', 'B', 'C', 'B', 'D', 'A', 'B']
        b = ['B', 'D', 'C', 'A', 'B', 'A']

        self.assertEqual(lcs.lcs_once_plus_const(a, b), len(['B', 'C', 'B', 'A']))
        self.assertEqual(lcs.lcs_twice(a, b), len(['B', 'C', 'B', 'A']))

    def test_example_two(self):
        a = list("CGATAATTGAGA")
        b = list("GTTCCTAATA")

        self.assertEqual(lcs.lcs_once_plus_const(a, b), len("CTAATA"))
        self.assertEqual(lcs.lcs_twice(a, b), len("CTAATA"))


if __name__ == '__main__':
    unittest.main()
