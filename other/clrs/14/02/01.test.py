import unittest
import os.path as path
import random

filename = path.join(path.dirname(__file__), '01.py')
exec(open(filename).read())


class OrderStatisticTreeTest(unittest.TestCase):
    def test_things(self):
        tree = Tree()

        two = tree.insert(2)

        self.assertEqual(tree.minimum, two)
        self.assertEqual(tree.maximum, two)

        three = tree.insert(3)
        self.assertEqual(tree.minimum, two)
        self.assertEqual(tree.maximum, three)
        self.assertEqual(tree.minimum.successor, three)

        one = tree.insert(1)
        self.assertEqual(tree.minimum, one)
        self.assertEqual(tree.minimum.successor, two)
        self.assertEqual(tree.minimum.successor.successor, three)

    def test_rank_when_inserting(self):
        k = 500
        numbers = list(range(1, k + 1))
        random.shuffle(numbers)

        tree = Tree()

        for n in numbers:
            tree.insert(n)

        for n in range(1, k + 1):
            self.assertEqual(tree.select(n).key, n)
            self.assertEqual(tree.search(n).rank(), n)

    def test_rank_when_deleting(self):
        k = 1000
        m = 500

        numbers = list(range(1, k + 1))
        random.shuffle(numbers)

        tree = Tree()

        for n in numbers:
            tree.insert(n)

        random.shuffle(numbers)
        remaining = numbers[0:m]

        for n in numbers[m:]:
            tree.delete(n)

        remaining.sort()

        for (i, n) in enumerate(remaining):
            i += 1
            self.assertEqual(tree.select(i).key, n)
            self.assertEqual(tree.search(n).rank(), i)

    def generate(self, m, n):
        numbers = list(range(m))
        random.shuffle(numbers)
        return numbers[0:n]

    def assertContains(self, tree, numbers):
        for n in numbers:
            self.assertIsNotNone(tree.search(n), f"should contain {n}")
            self.assertEqual(tree.search(n).key, n)

    def assertProperties(self, tree):
        heights = set()
        for n in tree.nodes():
            if not n.left or not n.right:
                heights.add(n.black_height())

            if n.color == Color.RED:
                self.assertEqual(n.left.color, Color.BLACK)
                self.assertEqual(n.right.color, Color.BLACK)

        self.assertEqual(len(heights), 1)
        self.assertEqual(tree.root.color, Color.BLACK)

    def assertForwardWalk(self, tree, numbers):
        prev = None
        node = tree.minimum

        for n in sorted(numbers):
            self.assertEqual(n, node.key)
            prev = node
            node = node.successor

        self.assertEqual(prev, tree.maximum)

    def assertBackwardWalk(self, tree, numbers):
        prev = None
        node = tree.maximum

        for n in reversed(sorted(numbers)):
            self.assertEqual(n, node.key)
            prev = node
            node = node.predecessor

        self.assertEqual(prev, tree.minimum)

    def test_insertions(self):
        numbers = self.generate(300, 100)

        tree = Tree()

        for n in numbers:
            tree.insert(n)

        self.assertContains(tree, numbers)
        self.assertForwardWalk(tree, numbers)
        self.assertBackwardWalk(tree, numbers)

    def test_properties(self):
        numbers = self.generate(300, 100)
        tree = Tree()

        for n in numbers:
            tree.insert(n)

        self.assertProperties(tree)
        self.assertForwardWalk(tree, numbers)
        self.assertBackwardWalk(tree, numbers)

    def test_deletion(self):
        numbers = self.generate(1000, 500)
        removed = numbers[:]
        random.shuffle(removed)
        removed = removed[0:250]
        remaining = list(set(numbers) - set(removed))

        tree = Tree()

        for n in numbers:
            tree.insert(n)

        for n in removed:
            tree.delete(n)

        self.assertContains(tree, remaining)

        for n in removed:
            self.assertIsNone(tree.search(n))

        self.assertProperties(tree)
        self.assertForwardWalk(tree, remaining)
        self.assertBackwardWalk(tree, remaining)


if __name__ == '__main__':
    unittest.main()
