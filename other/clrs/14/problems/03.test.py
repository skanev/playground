import unittest

bts = __import__('03').bts


class BitonicTravellingSalespersonTest(unittest.TestCase):
    def test_lcs(self):
        points = [
            (0, 0),
            (2, 3),
            (5, 2),
            (7, 1),
            (8, 4),
            (6, 5),
            (1, 6),
        ]

        expected = 0

        for i in range(0, len(points)):
            a, b = complex(*points[i - 1]), complex(*points[i])
            expected += abs(a - b)

        self.assertEqual(expected, bts(points))


if __name__ == '__main__':
    unittest.main()
