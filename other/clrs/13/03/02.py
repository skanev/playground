from enum import Enum


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


class Tree:
    def __init__(self):
        self.nil = Node(Color.BLACK, None, None, None, None)
        self.nil.parent = self.nil
        self.nil.left = self.nil
        self.nil.right = self.nil

        self.root = self.nil

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

        self.fixup(z)

    def fixup(self, z):
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
