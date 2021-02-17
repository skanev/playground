import unittest
import os.path as path
import random

filename = path.join(path.dirname(__file__), '03.py')
exec(open(filename).read())


class AVLTreeTest(unittest.TestCase):
    def assertBinarySearchTreeProperties(self, tree):
        for node in tree.nodes():
            if node.left:
                self.assertTrue(node.left.key < node.key)
            if node.right:
                self.assertTrue(node.right.key > node.key)

    def assertAVLProperties(self, tree):
        def calculated_height(node):
            return node.calculated_height if node else 0

        def height(node):
            return node.height if node else 0

        def calculate_height(node):
            if node.left:
                calculate_height(node.left)
            if node.right:
                calculate_height(node.right)

            node.calculated_height = 1 + max(calculated_height(node.left), calculated_height(node.right))

        calculate_height(tree.root)

        for node in tree.nodes():
            self.assertEqual(node.height, node.calculated_height)
            self.assertTrue(abs(height(node.left) - height(node.right)) <= 1)

    def testInsertionWithLeftRotation(self):
        tree = AVL()
        tree.insert(1)
        tree.insert(2)
        tree.insert(3)

        self.assertEqual(tree.root.height, 2)
        self.assertEqual(tree.root.key, 2)
        self.assertAVLProperties(tree)

    def testInsertionWithLeftRightRotation(self):
        tree = AVL()
        tree.insert(1)
        tree.insert(3)
        tree.insert(2)

        self.assertEqual(tree.root.height, 2)
        self.assertEqual(tree.root.key, 2)
        self.assertAVLProperties(tree)

    def testInsertionWithRightRotation(self):
        tree = AVL()
        tree.insert(3)
        tree.insert(2)
        tree.insert(1)

        self.assertEqual(tree.root.height, 2)
        self.assertEqual(tree.root.key, 2)
        self.assertAVLProperties(tree)

    def testInsertionWithRightLeftRotation(self):
        tree = AVL()
        tree.insert(3)
        tree.insert(1)
        tree.insert(2)

        self.assertEqual(tree.root.height, 2)
        self.assertEqual(tree.root.key, 2)
        self.assertAVLProperties(tree)

    def testInsertion(self):
        tree = AVL()
        tree.insert(1)
        tree.insert(3)
        tree.insert(5)
        tree.insert(7)
        tree.insert(6)
        tree.insert(4)
        tree.insert(2)
        tree.insert(8)
        tree.insert(9)

        for i in range(1, 10):
            self.assertIsNotNone(tree.search(i))

        self.assertIsNone(tree.search(0))
        self.assertBinarySearchTreeProperties(tree)
        self.assertAVLProperties(tree)

    def testRandomInsertion(self):
        numbers = list(range(0, 500))
        random.shuffle(numbers)
        numbers = numbers[:250]

        tree = AVL()
        for i in numbers:
            tree.insert(i)

        for i in numbers:
            self.assertIsNotNone(tree.search(i))

        self.assertBinarySearchTreeProperties(tree)
        self.assertAVLProperties(tree)


if __name__ == '__main__':
    unittest.main()
