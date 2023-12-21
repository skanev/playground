import unittest
import random
import functools

neat_print = __import__('04').neat_print

def naive(words, limit):
    @functools.cache
    def possible(start):
        if start == len(words):
            return [[]]

        line = []

        result = []

        while True:
            if start == len(words): break

            line.append(words[start])
            start += 1

            if sum(line) + len(line) - 1 > limit: break

            for rest in possible(start):
                result.append([line.copy(), *rest])

        return result

    def score(lines):
        return sum((limit - sum(line) - len(line) + 1) ** 3 for line in lines[:-1])

    return min(score(lines) for lines in possible(0))


class PrintingNeatlyTest(unittest.TestCase):
    def test_printing_neatly(self):
        random.seed(0)
        count = 16
        limit = 30
        words = [random.randint(1, 10) for _ in range(count)]

        self.assertEqual(neat_print(words, limit), naive(words, limit))

if __name__ == '__main__':
    unittest.main()
