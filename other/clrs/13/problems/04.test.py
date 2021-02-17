import unittest
import os.path as path
import random

filename = path.join(path.dirname(__file__), '04.py')
exec(open(filename).read())


class TreapTest(unittest.TestCase):
    def assertBinarySearchTreeProperties(self, tree):
        for node in tree.nodes():
            if node.left:
                self.assertTrue(node.left.key < node.key)
            if node.right:
                self.assertTrue(node.right.key > node.key)

    def assertTreapProperties(self, treap):
        for node in treap.nodes():
            if node.left:
                self.assertTrue(node.priority < node.left.priority)
            if node.right:
                self.assertTrue(node.priority < node.right.priority)

    def testInsertion(self):
        treap = Treap()
        treap.insert('B', priority=7)
        treap.insert('H', priority=5)
        treap.insert('G', priority=4)

        self.assertIsNotNone(treap.search('B'))
        self.assertIsNotNone(treap.search('H'))
        self.assertIsNotNone(treap.search('G'))

    def testSameTreapWithExample(self):
        nodes = [
            ('G', 4), ('B', 7), ('A', 10), ('E', 23), ('H', 5), ('K', 65),
            ('I', 73), ('C', 25), ('D', 9), ('F', 2)
        ]

        def make_treap(nodes):
            treap = Treap()
            for (key, priority) in nodes:
                treap.insert(key, priority=priority)
            return treap


        treap = make_treap(nodes)
        first = str(treap)

        for _ in range(100):
            random.shuffle(nodes)
            another = str(make_treap(nodes))
            self.assertEqual(first, another)

    def testRandomTreap(self):
        k = 300

        elements = list(range(k))
        priorities = list(range(k))
        random.shuffle(elements)
        random.shuffle(priorities)

        treap = Treap()
        for (key, priority) in zip(elements, priorities):
            treap.insert(key, priority=priority)

        for n in elements:
            self.assertIsNotNone(treap.search(n))

        self.assertBinarySearchTreeProperties(treap)
        self.assertTreapProperties(treap)



if __name__ == '__main__':
    unittest.main()
