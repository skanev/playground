import unittest
import os.path as path
import random

filename = path.join(path.dirname(__file__), '01.py')
exec(open(filename).read())


def Red(key, left=None, right=None):
    return Node(key=key, left=left, right=right, color=Color.RED)


def Black(key, left=None, right=None):
    return Node(key=key, left=left, right=right, color=Color.BLACK)


class RedBlackTest(unittest.TestCase):
    def generate(self, m, n):
        numbers = list(range(m))
        random.shuffle(numbers)
        return numbers[0:n]

    def assertContains(self, tree, numbers):
        for n in numbers:
            self.assertIsNotNone(tree.search(n), f"should contain {n}")
            self.assertEqual(tree.search(n).key, n)

    def assertDoesNotContain(self, tree, numbers):
        for n in numbers:
            self.assertIsNone(tree.search(n), f"should not contain {n}")

    def assertInAndOut(self, tree, included, excluded):
        self.assertContains(tree, included)
        self.assertDoesNotContain(tree, excluded)

    def assertProperties(self, tree):
        heights = set()
        for n in tree.nodes():
            if n.isRed():
                if n.left:
                    self.assertEqual(n.left.color, Color.BLACK)
                if n.right:
                    self.assertEqual(n.right.color, Color.BLACK)

        self.assertEqual(len(tree.black_heights()), 1)

        if tree.root:
            self.assertEqual(tree.root.color, Color.BLACK)

    def test_simple_insertion(self):
        tree = Tree()

        tree = tree.insert(4)
        tree = tree.insert(3)
        tree = tree.insert(5)

        modified = tree.insert(1)

        self.assertIsNotNone(modified.search(1))
        self.assertIsNotNone(modified.search(4))
        self.assertIsNotNone(modified.search(3))
        self.assertIsNotNone(modified.search(5))

        self.assertIsNone(tree.search(1))
        self.assertIsNotNone(tree.search(4))
        self.assertIsNotNone(tree.search(3))
        self.assertIsNotNone(tree.search(5))

    def test_exercise_insertion_properties(self):
        tree = Tree()

        tree = tree.insert(41)
        tree = tree.insert(38)
        tree = tree.insert(31)
        tree = tree.insert(12)
        tree = tree.insert(19)
        tree = tree.insert(8)

        self.assertProperties(tree)

    def test_insertions(self):
        numbers = self.generate(300, 100)
        sequences = []
        missing = []
        trees = []
        sofar = []

        tree = Tree()

        for n in numbers:
            sofar.append(n)
            sequences.append(sofar[:])
            missing.append(list(set(numbers) - set(sofar)))
            tree = tree.insert(n)
            trees.append(tree)

        for (tree, included, excluded) in zip(trees, sequences, missing):
            self.assertContains(tree, included)
            self.assertDoesNotContain(tree, excluded)
            self.assertProperties(tree)

        nodes = []
        for tree in trees:
            nodes += tree.nodes()

        self.assertTrue(len(set(nodes)) < len(nodes))

    def test_delete_empty(self):
        tree = Tree()
        tree = tree.insert(1)
        tree = tree.delete(1)

    def test_exercise_deletion_properties(self):
        tree = Tree()

        tree = tree.insert(41)
        tree = tree.insert(38)
        tree = tree.insert(31)
        tree = tree.insert(12)
        tree = tree.insert(19)
        tree = tree.insert(8)

        first  = tree.delete(8)
        second = first.delete(12)
        third  = second.delete(19)
        fourth = third.delete(31)
        fifth  = fourth.delete(38)

        self.assertInAndOut(first,  [12, 19, 31, 38, 41], [8])
        self.assertInAndOut(second, [19, 31, 38, 41], [8, 12])
        self.assertInAndOut(third,  [31, 38, 41], [8, 12, 19])
        self.assertInAndOut(fourth, [38, 41], [8, 12, 19, 31])
        self.assertInAndOut(fifth,  [41], [8, 12, 19, 31, 38])

    def test_delete_fixup_case_1(self):
        subtree = Red("B",
                left=Black("A"),
                right=Red("D",
                    left=Black("C"),
                    right=Black("E")))
        tree = Tree(root=subtree)
        top = tree.delete_fixup([tree.root, tree.root.left])

        self.assertEqual(top.key, "D")
        self.assertEqual(top.left.key, "B")
        self.assertEqual(top.left.left.key, "A")
        self.assertEqual(top.left.right.key, "C")
        self.assertEqual(top.right.key, "E")

    def test_delete_fixup_case_2(self):
        subtree = Red("B",
                left=Black("A"),
                right=Black("D",
                    left=Black("C"),
                    right=Black("E")))
        tree = Tree(root=subtree)
        top = tree.delete_fixup([tree.root, tree.root.left])

        self.assertEqual(top.key, "B")
        self.assertEqual(top.left.key, "A")
        self.assertEqual(top.right.key, "D")
        self.assertEqual(top.right.left.key, "C")
        self.assertEqual(top.right.right.key, "E")

    def test_delete_fixup_case_3(self):
        subtree = Red("B",
                left=Black("A"),
                right=Black("D",
                    left=Red("C"),
                    right=Black("E")))
        tree = Tree(root=subtree)

        top = tree.delete_fixup([tree.root, tree.root.left])
        self.assertEqual(top.key, "C")
        self.assertEqual(top.left.key, "B")
        self.assertEqual(top.left.left.key, "A")
        self.assertEqual(top.right.key, "D")
        self.assertEqual(top.right.right.key, "E")


    def test_delete_fixup_case_4(self):
        subtree = Red("B",
                left=Black("A"),
                right=Black("D",
                    left=Red("C"),
                    right=Red("E")))
        tree = Tree(root=subtree)

        new_root = tree.delete_fixup([tree.root, tree.root.left])

        top = new_root
        self.assertEqual(top.key, "D")
        self.assertEqual(top.color, Color.RED)
        self.assertEqual(top.left.key, "B")
        self.assertEqual(top.left.color, Color.BLACK)
        self.assertEqual(top.left.left.key, "A")
        self.assertEqual(top.left.left.color, Color.BLACK)
        self.assertEqual(top.left.right.key, "C")
        self.assertEqual(top.left.right.color, Color.RED)
        self.assertEqual(top.right.key, "E")
        self.assertEqual(top.right.color, Color.BLACK)

    def test_deletion(self):
        numbers = self.generate(300, 100)
        removed = numbers[:]

        tree = Tree()
        for n in numbers:
            tree = tree.insert(n)

        trees = []
        ins = []
        outs = []
        k = 100

        random.shuffle(numbers)
        removed = []

        for _ in range(k):
            n = numbers.pop()
            removed.append(n)
            tree = tree.delete(n)
            self.assertProperties(tree)
            ins.append(numbers[:])
            outs.append(removed[:])
            trees.append(tree)

        for (tree, included, excluded) in zip(trees, ins, outs):
            self.assertInAndOut(tree, included, excluded)
            self.assertProperties(tree)

        nodes = []
        for tree in trees:
            nodes += tree.nodes()

        self.assertTrue(len(set(nodes)) < len(nodes))

if __name__ == '__main__':
    unittest.main()
