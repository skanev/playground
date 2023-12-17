import unittest

mono = __import__('05').mono


class LongestMonotonicallyIncreasingSubsequenceTest(unittest.TestCase):
    def test_examples(self):
        self.assertEqual(
            mono([1, 2, 3, 10, 11, 4, 5, 8, 6, 7, 1]),
            [1, 2, 3, 4, 5, 6, 7]
        )

        self.assertEqual(
            mono([8, 2, 4, 2]),
            [2, 2]
        )


if __name__ == '__main__':
    unittest.main()
