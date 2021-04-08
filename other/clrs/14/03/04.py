from enum import Enum
from collections import deque


class Interval:
    def __init__(self, low, high):
        assert low <= high
        self.low = low
        self.high = high

    def __eq__(self, other):
        return isinstance(other, Interval) and self.low == other.low and \
            self.high == other.high

    def __hash__(self):
        return hash((self.low, self.high))

    def __contains__(self, n):
        return self.low <= n <= self.high

    def __repr__(self):
        return f"Interval({self.low}, {self.high})"

    __str__ = __repr__

    def overlaps(self, other):
        return self.low <= other.high and other.low <= self.high


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

def max_maybe(*args):
    return max([arg for arg in args if arg is not None])

class Node:
    def __init__(self, color, interval, parent, left, right, max, tree):
        self.color = color
        self.interval = interval
        self.parent = parent
        self.left = left
        self.right = right
        self.tree = tree
        self.max = max

    def sexp(self):
        if self.isNil():
            return 'NIL'

        color = 'R' if self.color == Color.RED else 'B'

        return f"{color}({self.interval}, max={self.max}, {self.left}, {self.right})"

    __str__ = sexp

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
        return self.interval is NIL_KEY

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

        self.max = max_maybe(
            self.interval.high,
            self.left.max if self.left else None,
            self.right.max if self.right else None,
        )

        child.max = max_maybe(
            child.interval.high,
            child.left.max if child.left else None,
            child.right.max if child.right else None,
        )

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


nil = Node(Color.BLACK, NIL_KEY, None, None, None, None, None)
nil.parent = nil
nil.left = nil
nil.right = nil


class IntervalTree:
    def __init__(self):
        self.root = nil

    def __str__(self):
        return self.root.sexp()

    def find(self, interval):
        node = self.root

        while node:
            if node.interval == interval:
                return node
            elif interval.low < node.interval.low:
                node = node.left
            else:
                node = node.right

        return None

    def search(self, interval):
        node = self.root

        while node:
            if interval.overlaps(node.interval):
                return node
            elif node.left and node.left.max >= interval.low:
                node = node.left
            else:
                node = node.right

        return None

    def search_all(self, interval):
        result = []

        def collect(node):
            if node.interval.overlaps(interval):
                result.append(node.interval)

            if node.left and interval.low <= node.left.max:
                collect(node.left)

            if node.right and Interval(node.interval.low,
                    node.right.max).overlaps(interval):
                collect(node.right)

        collect(self.root)

        return result

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

    def insert(self, interval):
        new = Node(Color.RED, interval, None, None, None, interval.high, self)
        parent = nil
        node = self.root
        while node:
            parent = node
            if new.interval.low < node.interval.low:
                node = node.left
            else:
                node = node.right

        new.parent = parent

        if not parent:
            self.root = new
        elif new.interval.low < parent.interval.low:
            parent.left = new
        else:
            parent.right = new

        new.set(left=nil, right=nil, color=Color.RED)

        self.max_fixup(parent)

        self.insert_fixup(new)

    def max_fixup(self, node):
        while node:
            node.max = max_maybe(
                node.interval.high,
                node.left.max if node.left else None,
                node.right.max if node.right else None
            )

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

    def delete(self, interval):
        deleted = self.find(interval)
        y = deleted
        y_original_color = y.color

        if not deleted.left:
            extra_black = deleted.right
            deleted.transplant(deleted.right)
            self.max_fixup(deleted)
        elif not deleted.right:
            extra_black = deleted.left
            deleted.transplant(deleted.left)
            self.max_fixup(deleted)
        else:
            y = deleted.right.minimum()
            y_original_color = y.color
            extra_black = y.right

            todo = None

            if y.parent is deleted:
                extra_black.parent = y
            else:
                todo = y.parent
                y.transplant(y.right)
                y.right = deleted.right
                y.right.parent = y

            deleted.transplant(y)
            y.left = deleted.left
            y.left.parent = y
            y.color = deleted.color

            self.max_fixup(todo or y)

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
