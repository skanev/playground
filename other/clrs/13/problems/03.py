from collections import deque


class Node:
    def __init__(self, key, height=-1, left=None, right=None):
        self.key = key
        self.height = height
        self.left = left
        self.right = right

    def __str__(self):
        def dump(node):
            if not node:
                return "NIL"
            else:
                return f"/{node.height}/({node.key}, {dump(node.left)}, {dump(node.right)})"

        return dump(self)

    def left_rotate(self):
        child = self.right
        assert(child)
        self.right = child.left
        child.left = self

        self.height = 1 + max(height(self.left), height(self.right))
        child.height = 1 + max(height(child.left), height(child.right))

        return child

    def right_rotate(self):
        child = self.left
        assert(child)
        self.left = child.right
        child.right = self

        self.height = 1 + max(height(self.left), height(self.right))
        child.height = 1 + max(height(child.left), height(child.right))

        return child

    __repr__ = __str__


def height(node):
    return node.height if node else 0


class AVL:
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

    def insert(self, key):
        def insert_node(subtree, node):
            if not subtree:
                return node
            elif node.key < subtree.key:
                subtree.left = insert_node(subtree.left, node)
            else:
                subtree.right = insert_node(subtree.right, node)

            subtree.height = 1 + max(height(subtree.left), height(subtree.right))

            balance = height(subtree.left) - height(subtree.right)

            if balance < -1:
                if key < subtree.right.key:
                    subtree.right = subtree.right.right_rotate()
                return subtree.left_rotate()
            elif balance > 1:
                if key > subtree.left.key:
                    subtree.left = subtree.left.left_rotate()
                return subtree.right_rotate()
            else:
                return subtree

        new = Node(key, height=1)
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
