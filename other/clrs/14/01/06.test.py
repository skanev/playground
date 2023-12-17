import unittest

fibonacci = __import__('06').fibonacci


class FibonacciTest(unittest.TestCase):
    def test_cutting_without_a_cost(self):
        self.assertEqual(fibonacci(1), 1)
        self.assertEqual(fibonacci(2), 1)
        self.assertEqual(fibonacci(3), 2)
        self.assertEqual(fibonacci(4), 3)
        self.assertEqual(fibonacci(5), 5)
        self.assertEqual(fibonacci(6), 8)
        self.assertEqual(fibonacci(7), 13)
        self.assertEqual(fibonacci(8), 21)
        self.assertEqual(fibonacci(9), 34)
        self.assertEqual(fibonacci(10), 55)
        self.assertEqual(fibonacci(11), 89)
        self.assertEqual(fibonacci(12), 144)


if __name__ == '__main__':
    unittest.main()
