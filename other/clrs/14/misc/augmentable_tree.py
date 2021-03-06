from enum import Enum
from collections import deque


class Color(Enum):
    RED = 1
    BLACK = 2


NIL_KEY = object()


def other(direction):
    if direction == 'left':
        return 'right'
    elif direction == 'right':
        return 'left'
    else:
        assert(False)


class Node:
    def __init__(self, color, key, parent, left, right, tree):
        self.color = color
        self.key = key
        self.parent = parent
        self.left = left
        self.right = right
        self.tree = tree

    def sexp(self):
        if self.isNil():
            return 'NIL'

        color = 'R' if self.color == Color.RED else 'B'

        return f"{color}({self.key}, {self.left}, {self.right})"

    def __str__(self):
        return self.sexp()

    def black_height(self):
        node = self
        height = 0

        while node is not nil:
            if node.color == Color.BLACK:
                height += 1
            node = node.parent

        return height

    def isRed(self):
        return self.color == Color.RED

    def isBlack(self):
        return self.color == Color.BLACK

    def isNil(self):
        return self.key is NIL_KEY

    def isNotNil(self):
        return not self.isNil()

    def __bool__(self):
        return self.isNotNil()

    def child(self, direction):
        if direction == 'left':
            return self.left
        elif direction == 'right':
            return self.right
        else:
            assert(False)

    def set_child(self, direction, child):
        if direction == 'left':
            self.left = child
        elif direction == 'right':
            self.right = child
        else:
            assert(False)

    __getitem__ = child
    __setitem__ = set_child

    def other(self, direction):
        return self.child(other(direction))

    def rotate(self, direction):
        child = self.other(direction)
        self[other(direction)] = child[direction]

        if child[direction]:
            child[direction].parent = self

        child.parent = self.parent

        if not self.parent:
            self.tree.root = child
        elif self is self.parent[direction]:
            self.parent[direction] = child
        else:
            self.parent[other(direction)] = child

        child[direction] = self
        self.parent = child

        self.tree.recalculate_node(self)
        self.tree.recalculate_node(child)

    def left_rotate(self):
        self.rotate('left')

    def right_rotate(self):
        self.rotate('right')

    def transplant(self, other):
        if not self.parent:
            self.tree.root = other
        elif self is self.parent.left:
            self.parent.left = other
        else:
            self.parent.right = other
        other.parent = self.parent

    def set(self, parent=None, left=None, right=None, color=None):
        if color:
            self.color = color
        if left is not None:
            self.left = left
        if right is not None:
            self.right = right
        if parent is not None:
            self.parent = parent

    def minimum(self):
        node = self

        while node.left:
            node = node.left

        return node


nil = Node(Color.BLACK, NIL_KEY, None, None, None, None)
nil.parent = nil
nil.left = nil
nil.right = nil


class AugmentableTree:
    def __init__(self):
        self.root = nil

    def __str__(self):
        return self.root.sexp()

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

    def nodes(self):
        items = deque()

        if self.root:
            items.append(self.root)

        while items:
            node = items.popleft()

            yield node

            if node.left:
                items.append(node.left)

            if node.right:
                items.append(node.right)

    def insert(self, key):
        new = Node(Color.RED, key, None, None, None, self)
        self.augment_node(new)

        parent = nil
        node = self.root
        while node:
            parent = node
            if new.key < node.key:
                node = node.left
            else:
                node = node.right

        new.parent = parent

        if not parent:
            self.root = new
        elif new.key < parent.key:
            parent.left = new
        else:
            parent.right = new

        new.set(left=nil, right=nil, color=Color.RED)

        self.recalculate_ancestors(parent)

        self.insert_fixup(new)

    def recalculate_ancestors(self, node):
        while node:
            self.recalculate_node(node)
            node = node.parent

    def insert_fixup(self, node):
        while node.parent.isRed():
            if node.parent is node.parent.parent.left:
                direction = 'left'
            else:
                direction = 'right'

            if direction == 'left' or direction == 'right':
                uncle = node.parent.parent[other(direction)]
                if uncle.isRed():
                    node.parent.color = Color.BLACK
                    uncle.color = Color.BLACK
                    node.parent.parent.color = Color.RED
                    node = node.parent.parent
                else:
                    if node is node.parent[other(direction)]:
                        node = node.parent
                        node.rotate(direction)
                    node.parent.color = Color.BLACK
                    node.parent.parent.color = Color.RED
                    node.parent.parent.rotate(other(direction))

        self.root.color = Color.BLACK

    def delete(self, key):
        deleted = self.search(key)
        y = deleted
        y_original_color = y.color
        to_fix = deleted

        if not deleted.left:
            extra_black = deleted.right
            deleted.transplant(deleted.right)
        elif not deleted.right:
            extra_black = deleted.left
            deleted.transplant(deleted.left)
        else:
            y = deleted.right.minimum()
            y_original_color = y.color
            extra_black = y.right

            to_fix = y

            if y.parent is deleted:
                extra_black.parent = y
            else:
                to_fix = y.parent
                y.transplant(y.right)
                y.right = deleted.right
                y.right.parent = y

            deleted.transplant(y)
            y.left = deleted.left
            y.left.parent = y
            y.color = deleted.color

        self.recalculate_ancestors(to_fix)

        if y_original_color == Color.BLACK:
            self.delete_fixup(extra_black)

    def delete_fixup(self, node):
        while node is not self.root and node.isBlack():
            if node is node.parent.left:
                direction = 'left'
            else:
                direction = 'right'

            sibling = node.parent[other(direction)]

            if sibling.isRed():
                sibling.color = Color.BLACK
                node.parent.color = Color.RED
                node.parent.rotate(direction)
                sibling = node.parent[other(direction)]

            if sibling.left.isBlack() and sibling.right.isBlack():
                sibling.color = Color.RED
                node = node.parent
            else:
                if sibling[other(direction)].isBlack():
                    sibling[direction].color = Color.BLACK
                    sibling.color = Color.RED
                    sibling.rotate(other(direction))
                    sibling = node.parent[other(direction)]

                sibling.color = node.parent.color
                node.parent.color = Color.BLACK
                sibling[other(direction)].color = Color.BLACK
                sibling.parent.rotate(direction)
                node = self.root

        node.color = Color.BLACK
