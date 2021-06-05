import unittest
import random

mono = __import__('06').mono


def quadratic(items):
    best = [1] * len(items)
    choices = [None] * len(items)

    for j in range(1, len(items)):
        for i in range(0, j):
            if items[i] <= items[j] and best[j] <= best[i] + 1:
                best[j] = best[i] + 1
                choices[j] = i

    n = max(range(0, len(items)), key=lambda x: (best[x], x))

    result = []

    while n is not None:
        result.append(items[n])
        n = choices[n]

    result.reverse()

    return result


class FasterLongestMonotonicallyIncreasingSubsequenceTest(unittest.TestCase):
    def test_examples(self):
        self.assertEqual(
            mono([1, 2, 8, 9, 3, 4, 5]),
            [1, 2, 3, 4, 5]
        )

        self.assertEqual(
            mono([1, 2, 3, 10, 11, 4, 5, 8, 6, 7, 1]),
            [1, 2, 3, 4, 5, 6, 7]
        )

        self.assertEqual(mono([6, 6, 0, 4]), [0, 4])

        self.assertEqual(mono([1, 2, 3, 4]), [1, 2, 3, 4])

    def test_comparison(self):
        k = 500
        n = 20
        random.seed(0)
        for a in range(n):
            sequence = list(map(lambda _: random.randint(0, 9), range(k)))
            self.assertEqual(len(quadratic(sequence)), len(mono(sequence)))


if __name__ == '__main__':
    unittest.main()
