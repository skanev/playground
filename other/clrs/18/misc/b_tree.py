class Node:
    def __init__(self, keys = None, children = None):
        self.keys = keys or []
        self.children = children

    def isLeaf(self):
        return not self.children

    def print(self, indent):
        print(" " * indent + repr(self.keys))
        if self.children:
            for child in self.children:
                child.print(indent + 2)

    def insert(self, i, key, child):
        assert not self.isLeaf()

        self.keys.insert(i, key)
        self.children.insert(i + 1, child)

    def merge(self, i):
        left = self.children[i]
        right = self.children[i + 1]

        left.keys.append(self.keys[i])
        left.keys.extend(right.keys)

        if not left.isLeaf():
            left.children.extend(right.children)

        self.keys.pop(i)
        self.children.pop(i + 1)

    def min(self):
        if self.isLeaf():
            return self.keys[0]
        else:
            return self.children[0].min()

    def max(self):
        if self.isLeaf():
            return self.keys[-1]
        else:
            return self.children[-1].max()

    def truncate(self, size):
        self.keys = self.keys[:size]
        if not self.isLeaf():
            self.children = self.children[:size + 1]

    def index(self, k):
        i = 0

        while i < len(self.keys) and k > self.keys[i]:
            i += 1

        return i

    def split_at(self, i, t):
        assert len(self.keys) < 2 * t - 1
        assert len(self.children[i].keys) == 2 * t - 1

        child = self.children[i]
        node = Node(child.keys[t:], None if child.isLeaf() else child.children[t:])

        self.insert(i, child.keys[t - 1], node)
        child.truncate(t - 1)

    def delete(self, k, t):
        if self.isLeaf():
            self.keys.remove(k)
        elif k in self.keys:
            i = self.index(k)

            n_keys = len(self.children[i].keys)
            child = self.children[i]
            right = self.children[i + 1]

            if n_keys >= t:
                pred = child.max()
                self.keys[i] = pred
                child.delete(pred, t)
            elif n_keys == t - 1 and len(right.keys) >= t:
                succ = right.min()
                self.keys[i] = succ
                right.delete(succ, t)
            elif n_keys == t - 1 and len(right.keys) == t - 1:
                self.merge(i)
                self.children[i].delete(k, t)
            else:
                raise Exception("unreachable")
        else:
            i = self.index(k)
            child = self.children[i]
            n = len(child.keys)

            left = self.children[i - 1] if i else None
            right = self.children[i + 1] if i < len(self.keys) else None

            if n >= t:
                child.delete(k, t)
            elif left and len(left.keys) >= t:
                child.keys.insert(0, self.keys[i - 1])

                if not child.isLeaf():
                    child.children.insert(0, left.children[-1])

                self.keys[i - 1] = left.keys[-1]
                left.keys.pop()

                if not left.isLeaf():
                    left.children.pop()

                child.delete(k, t)
            elif right and len(right.keys) >= t:
                child.keys.append(self.keys[i])

                if not child.isLeaf():
                    child.children.append(right.children[0])

                self.keys[i] = right.keys[0]
                right.keys.pop(0)

                if not right.isLeaf():
                    right.children.pop(0)

                child.delete(k, t)
            elif left and len(left.keys) == t - 1:
                self.merge(i - 1)
                self.children[i - 1].delete(k, t)
            elif right and len(right.keys) == t - 1:
                self.merge(i)
                self.children[i].delete(k, t)
            else:
                raise Exception("unreachable")

    def __bool__(self):
        return bool(self.keys or self.children)

    def __len__(self):
        return len(self.keys) + sum(len(c) for c in self.children) if self.children else 0

    def __contains__(self, k):
        i = self.index(k)

        return i < len(self.keys) and self.keys[i] == k or \
                not self.isLeaf() and k in self.children[i]

    def __iter__(self):
        for i, key in enumerate(self.keys):
            if self.children:
                yield from self.children[i]

            yield key

        if self.children:
            yield from self.children[-1]


class BTree:
    def __init__(self, t):
        self.t = t
        self.root = Node()

    def insert(self, k):
        if len(self.root.keys) == 2 * self.t - 1:
            self.root = Node([], [self.root])
            self.root.split_at(0, self.t)

        self.__insert_non_full(self.root, k)

    def delete(self, k):
        self.root.delete(k, self.t)

        if not self.root.keys and self.root.children:
            self.root = self.root.children[0]

    def __len__(self):
        return len(self.root)

    def __insert_non_full(self, x, k):
        assert len(x.keys) < 2 * self.t - 1

        i = x.index(k)

        if x.isLeaf():
            x.keys.insert(i, k)
        else:
            if len(x.children[i].keys) == 2 * self.t - 1:
                x.split_at(i, self.t)

                if k > x.keys[i]:
                    i += 1

            self.__insert_non_full(x.children[i], k)

    def __contains__(self, k):
        return k in self.root

    def __iter__(self):
        return iter(self.root)

    def print(self):
        print()
        self.root.print(0)


tree = BTree(2)

import unittest
import random

class BTreeTest(unittest.TestCase):
    def generate(self, m, n):
        numbers = list(range(m))
        random.shuffle(numbers)
        return numbers[0:n]

    def assertContains(self, tree, numbers):
        for n in numbers:
            self.assertIsNotNone(tree.search(n), f"should contain {n}")
            self.assertEqual(tree.search(n).key, n)

    # def assertProperties(self, tree):
        # heights = set()
        # for n in tree.nodes():
            # if not n.left or not n.right:
                # heights.add(n.black_height())

            # if n.color == Color.RED:
                # self.assertEqual(n.left.color, Color.BLACK)
                # self.assertEqual(n.right.color, Color.BLACK)

        # self.assertEqual(len(heights), 1)
        # self.assertEqual(tree.root.color, Color.BLACK)

    def test_insertions(self):
        limit = 1000

        for t in range(2, 10):
            numbers = self.generate(limit, 500)

            tree = BTree(t)
            for n in numbers:
                tree.insert(n)
                self.assertEqual(list(tree), sorted(list(tree)))

            numbers = set(numbers)

            for i in range(0, limit):
                self.assertEqual(i in tree, i in numbers)

    def test_deletions(self):
        limit = 1000

        for t in range(4, 10):
            numbers = self.generate(limit, 500)

            tree = BTree(t)

            for n in numbers:
                tree.insert(n)

            number_set = set(numbers)

            random.shuffle(numbers)

            for i in numbers:
                self.assertTrue(i in tree)
                tree.delete(i)
                number_set.remove(i)

                # print(f"Deleted {i}")
                self.assertTrue(i not in tree)
                self.assertEqual(list(tree), sorted(list(tree)))
    # def test_properties(self):
        # numbers = self.generate(300, 100)
        # tree = Tree()

        # for n in numbers:
            # tree.insert(n)

        # self.assertProperties(tree)

    # def test_deletion(self):
        # numbers = self.generate(1000, 500)
        # removed = numbers[:]
        # random.shuffle(removed)
        # removed = removed[0:250]
        # remaining = list(set(numbers) - set(removed))

        # tree = Tree()

        # for n in numbers:
            # tree.insert(n)

        # for n in removed:
            # tree.delete(n)

        # self.assertContains(tree, remaining)

        # for n in removed:
            # self.assertIsNone(tree.search(n))

        # self.assertProperties(tree)


if __name__ == '__main__':
    unittest.main()
