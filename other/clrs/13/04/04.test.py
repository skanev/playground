import unittest
import os.path as path
import random

filename = path.join(path.dirname(__file__), '04.py')
exec(open(filename).read())


class RedBlackTest(unittest.TestCase):
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
            if n.left is tree.nil or n.right is tree.nil:
                heights.add(n.black_height(tree.nil))

            if n.color == Color.RED:
                self.assertEqual(n.left.color, Color.BLACK)
                self.assertEqual(n.right.color, Color.BLACK)

        self.assertEqual(len(heights), 1)
        self.assertEqual(tree.root.color, Color.BLACK)
        self.assertEqual(tree.nil.color, Color.BLACK)

    def test_insertions(self):
        numbers = self.generate(300, 100)

        tree = Tree()

        for n in numbers:
            tree.insert(n)

        self.assertContains(tree, numbers)

    def test_properties(self):
        numbers = self.generate(300, 100)
        tree = Tree()

        for n in numbers:
            tree.insert(n)

        self.assertProperties(tree)

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


if __name__ == '__main__':
    unittest.main()
