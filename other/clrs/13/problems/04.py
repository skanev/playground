from collections import deque


class Node:
    def __init__(self, key, priority, left=None, right=None):
        self.key = key
        self.priority = priority
        self.left = left
        self.right = right

    def __str__(self):
        def dump(node):
            if not node:
                return "NIL"
            else:
                return f"{node.key}:{node.priority}({dump(node.left)}, {dump(node.right)})"

        return dump(self)

    def left_rotate(self):
        child = self.right
        assert(child)
        self.right = child.left
        child.left = self

        return child

    def right_rotate(self):
        child = self.left
        assert(child)
        self.left = child.right
        child.right = self

        return child

    __repr__ = __str__


class Treap:
    def __init__(self):
        self.root = None

    def __str__(self):
        if not self.root:
            return "NIL"
        else:
            return str(self.root)

    __repr__ = __str__

    def nodes(self):
        if not self.root:
            return

        remaining = deque()
        remaining.append(self.root)

        while remaining:
            node = remaining.popleft()
            if node.left:
                remaining.append(node.left)
            if node.right:
                remaining.append(node.right)
            yield node

    def insert(self, key, priority=None):
        def insert_node(subtree, node):
            if not subtree:
                return node
            elif node.key < subtree.key:
                subtree.left = insert_node(subtree.left, node)
                if subtree.priority > subtree.left.priority:
                    subtree = subtree.right_rotate()
                return subtree
            else:
                subtree.right = insert_node(subtree.right, node)
                if subtree.priority > subtree.right.priority:
                    subtree = subtree.left_rotate()
                return subtree

        new = Node(key, priority=priority)
        self.root = insert_node(self.root, new)

    def search(self, key):
        node = self.root

        while node:
            if node.key == key:
                return node
            elif key < node.key:
                node = node.left
            else:
                node = node.right

        return None
