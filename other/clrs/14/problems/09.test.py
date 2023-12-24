import unittest
import random
import functools

@functools.cache
def naive(breakpoints, n):
    if not breakpoints: return 0
    return min(
            naive(breakpoints[:i], b) +
            naive(tuple(v - b for v in breakpoints[i+1:]), n - b) +
            n
            for (i, b) in enumerate(breakpoints))



break_string = __import__('09').break_string

def generate():
    limit = 100
    prob = 0.8
    step = 2
    breaks = tuple(n for n in range(1, limit, step) if random.random() > prob)
    return (breaks, limit)


class StringBreakingTest(unittest.TestCase):
    def test_breaking_string(self):
        for _ in range(10):
            breaks, limit = generate()
            expected = naive(breaks, limit)

            self.assertEqual(expected, break_string(breaks, limit))

if __name__ == '__main__':
    unittest.main()
