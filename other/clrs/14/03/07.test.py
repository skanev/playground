import unittest
import random
from os import path


filename = path.join(path.dirname(__file__), '07.py')
exec(open(filename).read())


def overlapper(rectangles):
    for i in range(0, len(rectangles) - 1):
        one, *others = rectangles[i:]
        for other in others:
            if other.overlaps(one):
                return one

    return None


class OverlapingRectanglesTest(unittest.TestCase):
    def test_simple_cases(self):
        self.assertFalse(
            overlap([
                Rectangle(left=0, right=10, top=0, bottom=10),
                Rectangle(left=20, right=30, top=0, bottom=10),
            ])
        )

        self.assertTrue(
            overlap([
                Rectangle(left=0, right=10, top=0, bottom=10),
                Rectangle(left=5, right=15, top=5, bottom=15),
            ])
        )

    def test_randomly_generated(self):
        n = 100
        s = 1000
        m = 150

        rectangles = []
        for i in range(0, n):
            left = random.randint(0, s - 2)
            right = random.randint(left + 1, min(s, left + m))
            top = random.randint(0, s - 2)
            bottom = random.randint(top + 1, min(s, top + m))
            rectangle = Rectangle(left=left, right=right, top=top, bottom=bottom)
            rectangles.append(rectangle)

        while target := overlapper(rectangles):
            self.assertTrue(overlap(rectangles))
            rectangles.remove(target)

        self.assertFalse(overlap(rectangles))

        random.shuffle(rectangles)

        while len(rectangles):
            self.assertFalse(overlap(rectangles))
            rectangles.pop()


if __name__ == '__main__':
    unittest.main()
