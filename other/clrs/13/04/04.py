from enum import Enum
from collections import deque


class Color(Enum):
    RED = 1
    BLACK = 2


class Node:
    def __init__(self, color, key, parent, left, right):
        self.color = color
        self.key = key
        self.parent = parent
        self.left = left
        self.right = right

    def sexp(self, nil):
        if self is nil:
            return 'NIL'

        color = 'R' if self.color == Color.RED else 'B'

        return f"{color}({self.key}, {self.left.sexp(nil)}, {self.right.sexp(nil)})"

    def black_height(self, nil):
        node = self
        height = 0

        while node is not nil:
            if node.color == Color.BLACK:
                height += 1
            node = node.parent

        return height


class Tree:
    def __init__(self):
        self.nil = Node(Color.BLACK, None, None, None, None)
        self.nil.parent = self.nil
        self.nil.left = self.nil
        self.nil.right = self.nil

        self.root = self.nil

    def __str__(self):
        return self.root.sexp(self.nil)

    def search(self, key):
        node = self.root

        while node is not self.nil:
            if node.key == key:
                return node
            elif key < node.key:
                node = node.left
            else:
                node = node.right

        return None

    def nodes(self):
        items = deque()

        if self.root is not self.nil:
            items.append(self.root)

        while items:
            node = items.popleft()

            yield node

            if node.left is not self.nil:
                items.append(node.left)

            if node.right is not self.nil:
                items.append(node.right)

    def insert(self, key):
        z = Node(Color.RED, key, None, None, None)
        y = self.nil
        x = self.root
        while x is not self.nil:
            y = x
            if z.key < x.key:
                x = x.left
            else:
                x = x.right

        z.parent = y

        if y is self.nil:
            self.root = z
        elif z.key < y.key:
            y.left = z
        else:
            y.right = z

        z.left = self.nil
        z.right = self.nil
        z.color = Color.RED

        self.insert_fixup(z)

    def insert_fixup(self, z):
        while z.parent.color == Color.RED:
            if z.parent is z.parent.parent.left:
                y = z.parent.parent.right
                if y.color == Color.RED:
                    z.parent.color = Color.BLACK
                    y.color = Color.BLACK
                    z.parent.parent.color = Color.RED
                    z = z.parent.parent
                else:
                    if z is z.parent.right:
                        z = z.parent
                        self.left_rotate(z)
                    z.parent.color = Color.BLACK
                    z.parent.parent.color = Color.RED
                    self.right_rotate(z.parent.parent)
            else:
                y = z.parent.parent.left
                if y.color == Color.RED:
                    z.parent.color = Color.BLACK
                    y.color = Color.BLACK
                    z.parent.parent.color = Color.RED
                    z = z.parent.parent
                else:
                    if z is z.parent.left:
                        z = z.parent
                        self.right_rotate(z)
                    z.parent.color = Color.BLACK
                    z.parent.parent.color = Color.RED
                    self.left_rotate(z.parent.parent)
        self.root.color = Color.BLACK

    def delete(self, z):
        z = self.search(z)
        y = z
        y_original_color = y.color
        if z.left is self.nil:
            x = z.right
            self.transplant(z, z.right)
        elif z.right is self.nil:
            x = z.left
            self.transplant(z, z.left)
        else:
            y = self.minimum(z.right)
            y_original_color = y.color
            x = y.right
            if y.parent is z:
                x.parent = y
            else:
                self.transplant(y, y.right)
                y.right = z.right
                y.right.parent = y
            self.transplant(z, y)
            y.left = z.left
            y.left.parent = y
            y.color = z.color

        if y_original_color == Color.BLACK:
            self.delete_fixup(x)

    def delete_fixup(self, x):
        while x is not self.root and x.color == Color.BLACK:
            if x is x.parent.left:
                w = x.parent.right

                if w.color == Color.RED:
                    w.color = Color.BLACK
                    x.parent.color = Color.RED
                    self.left_rotate(x.parent)
                    w = x.parent.right

                if w.left.color == Color.BLACK and w.right.color == Color.BLACK:
                    w.color = Color.RED
                    x = x.parent
                else:
                    if w.right.color == Color.BLACK:
                        w.left.color = Color.BLACK
                        w.color = Color.RED
                        self.right_rotate(w)
                        w = x.parent.right

                    w.color = x.parent.color
                    x.parent.color = Color.BLACK
                    w.right.color = Color.BLACK
                    self.left_rotate(w.parent)
                    x = self.root
            else:
                w = x.parent.left

                if w.color == Color.RED:
                    w.color = Color.BLACK
                    x.parent.color = Color.RED
                    self.right_rotate(x.parent)
                    w = x.parent.left

                if w.left.color == Color.BLACK and w.right.color == Color.BLACK:
                    w.color = Color.RED
                    x = x.parent
                else:
                    if w.left.color == Color.BLACK:
                        w.left.color = Color.BLACK
                        w.color = Color.RED
                        self.left_rotate(w)
                        w = x.parent.left

                    w.color = x.parent.color
                    x.parent.color = Color.BLACK
                    w.left.color = Color.BLACK
                    self.right_rotate(w.parent)
                    x = self.root

        x.color = Color.BLACK

    def minimum(self, node):
        while node.left is not self.nil:
            node = node.left

        return node

    def transplant(self, u, v):
        if u.parent is self.nil:
            self.root = v
        elif u is u.parent.left:
            u.parent.left = v
        else:
            u.parent.right = v
        v.parent = u.parent

    def left_rotate(self, x):
        y = x.right
        x.right = y.left

        if y.left is not self.nil:
            y.left.parent = x

        y.parent = x.parent

        if x.parent is self.nil:
            self.root = y
        elif x is x.parent.left:
            x.parent.left = y
        else:
            x.parent.right = y

        y.left = x
        x.parent = y

    def right_rotate(self, y):
        x = y.left
        y.left = x.right

        if x.right is not self.nil:
            x.right.parent = y

        x.parent = y.parent

        if y.parent is self.nil:
            self.root = x
        elif y is y.parent.left:
            y.parent.left = x
        else:
            y.parent.right = x

        x.right = y
        y.parent = x
