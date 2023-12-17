import unittest

lcs = __import__('03').lcs


class LongestCommonSubsequenceTest(unittest.TestCase):
    def test_lcs(self):
        a = list("CGATAATTGAGA")
        b = list("GTTCCTAATA")

        self.assertEqual(lcs(a, b), list("CTAATA"))


if __name__ == '__main__':
    unittest.main()
