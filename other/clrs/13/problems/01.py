from enum import Enum
from collections import deque


class Color(Enum):
    RED = 1
    BLACK = 2


UNCHANGED = object()


# In order to avoid duplicating symmetric code based on whether a child is left
# or right, we can work with directions instead. In order to be able to, we need
# to be able to flip a direction as well.
def other(direction):
    if direction == 'left':
        return 'right'
    elif direction == 'right':
        return 'left'
    else:
        assert(False)


def isBlackOrNil(node):
    return not node or node.isBlack()


class Node:
    def __init__(self, color, key, left=None, right=None):
        self.color = color
        self.key = key
        self.left = left
        self.right = right

    def __str__(self):
        return str(self.key)

    __repr__ = __str__

    def isRed(self):
        return self.color == Color.RED

    def isBlack(self):
        return self.color == Color.BLACK

    def child_direction(self, child):
        assert(child is not None)

        if self.left is child:
            return 'left'
        elif self.right is child:
            return 'right'
        else:
            assert(False)

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

    def with_replaced_child(self, replacement, child):
        assert(child is not None)

        if self.left is child:
            return self.copy(left=replacement)
        elif self.right is child:
            return self.copy(right=replacement)
        else:
            assert(False)

    def replace_child(self, replacement, child):
        assert(child is not None)

        if self.left is child:
            self.left = replacement
            return replacement
        elif self.right is child:
            self.right = replacement
            return replacement
        else:
            assert(False)

    def other(self, direction):
        return self.child(other(direction))

    def sexp(self):
        def sexp(node):
            if node:
                return node.sexp()
            else:
                return '_'

        color = 'R' if self.color == Color.RED else 'B'

        return f"{color}({self.key}, {sexp(self.left)}, {sexp(self.right)})"

    def copy(self, key=UNCHANGED, color=UNCHANGED, left=UNCHANGED, right=UNCHANGED):
        new = Node(self.color, self.key, self.left, self.right)

        if key is not UNCHANGED:
            new.key = key
        if color is not UNCHANGED:
            new.color = color
        if left is not UNCHANGED:
            new.left = left
        if right is not UNCHANGED:
            new.right = right

        return new

    def left_rotate(self):
        y = self.right
        return y.copy(left=self.copy(right=y.left))

    def right_rotate(self):
        x = self.left
        return x.copy(right=self.copy(left=x.right))

    def rotate(self, direction):
        if direction == 'left':
            return self.left_rotate()
        elif direction == 'right':
            return self.right_rotate()
        else:
            assert(False)

    # Returns the minimal node and the chain of ancestors that was traversed in
    # order to find it
    def minimum_with_ancestors(self):
        node = self
        ancestors = []

        while node.left:
            ancestors.append(node)
            node = node.left

        return (node, ancestors)


# Replaces a node at the bottom of an ancestor chains, and creates a new version
# of the chain where each parent is copied and updated to point to a newly
# created child.
#
# The final parent in `ancestors` should have `replaced` as a child. It creates
# a copy of the parent, replacing the child with `inserted` and then proceeds up
# the chain, updating every ancestor.
#
# At the end, it returns a new ancestor chain, where each node is a copy of the
# original, with an updated child.
def update_ancestor_chain(inserted, replaced, ancestors):
    ancestors = ancestors[:]
    result = [inserted]

    while ancestors:
        ancestor = ancestors.pop()
        inserted = ancestor.with_replaced_child(inserted, replaced)
        result.append(inserted)
        replaced = ancestor

    result.reverse()

    return result


class Tree:
    def __init__(self, root=None):
        self.root = root

    def __str__(self):
        if self.root:
            return self.root.sexp()
        else:
            return "NIL"

    __repr__ = __str__

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

    def black_heights(self):
        if not self.root:
            return {0}

        heights = set()
        left = deque()

        if self.root:
            left.append((self.root, 0))

        while left:
            (node, height) = left.popleft()
            if node.isBlack():
                height += 1

            if node.left:
                left.append((node.left, height))
            else:
                heights.add(height + 1)

            if node.right:
                left.append((node.right, height))
            else:
                heights.add(height + 1)

        return heights

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
        new = Node(Color.RED, key)
        parent = None
        current = self.root
        ancestors = []

        while current:
            parent = current
            ancestors.append(parent)

            if new.key < current.key:
                current = current.left
            else:
                current = current.right

        if ancestors:
            ancestors.pop()

        if not parent:
            return Tree(root=new.copy(color=Color.BLACK))
        elif new.key < parent.key:
            ancestors = update_ancestor_chain(parent.copy(left=new), parent, ancestors)
        else:
            ancestors = update_ancestor_chain(parent.copy(right=new), parent, ancestors)

        root = self.insert_fixup(new, ancestors)

        return Tree(root=root)

    def insert_fixup(self, current, ancestors):
        root = ancestors[0]

        while ancestors and ancestors[-1].isRed():
            parent = ancestors[-1]
            direction = ancestors[-2].child_direction(parent)
            grandfather = ancestors[-2]
            uncle = grandfather.other(direction)

            if uncle and uncle.isRed():
                parent.color = Color.BLACK
                grandfather.color = Color.RED
                grandfather.set_child(other(direction), uncle.copy(color=Color.BLACK))

                current = grandfather
                ancestors.pop()
                ancestors.pop()
            else:
                if current is parent.other(direction):
                    parent = parent.rotate(direction)
                    grandfather.set_child(direction, parent)
                    ancestors[-1] = parent
                    current = parent.child(direction)

                parent.color = Color.BLACK
                grandfather.color = Color.RED
                grandgrandfather = ancestors[-3] if len(ancestors) >= 3 else None
                parent = grandfather.rotate(other(direction))

                if not grandgrandfather:
                    root = parent
                else:
                    grandgrandfather.replace_child(parent, grandfather)

                break

        root.color = Color.BLACK

        return root

    def search_with_ancestors(self, key):
        node = self.root
        ancestors = []

        while node:
            if key == node.key:
                return (node, ancestors)
            elif key < node.key:
                ancestors.append(node)
                node = node.left
            else:
                ancestors.append(node)
                node = node.right

        return (None, None)

    def delete(self, key):
        deleted, ancestors = self.search_with_ancestors(key)
        original_color = deleted.color

        if not deleted.left and not deleted.right:
            ancestors = update_ancestor_chain(None, deleted, ancestors)
        elif not deleted.left:
            ancestors = update_ancestor_chain(deleted.right.copy(), deleted, ancestors)
        elif not deleted.right:
            ancestors = update_ancestor_chain(deleted.left.copy(), deleted, ancestors)
        else:
            moved, moved_ancestors = deleted.right.minimum_with_ancestors()

            original_color = moved.color
            extra_black = moved.right.copy() if moved.right else None
            span = []

            if moved_ancestors:
                span = update_ancestor_chain(extra_black, moved, moved_ancestors)
                moved = moved.copy(right=span[0])
                span.pop()

            elif moved.right:
                moved = moved.copy(right=extra_black)

            ancestors = update_ancestor_chain(
                    moved.copy(left=deleted.left, color=deleted.color),
                    deleted,
                    ancestors
                )

            ancestors += span
            ancestors.append(extra_black)

        root = ancestors[0]

        if original_color == Color.BLACK:
            root = self.delete_fixup(ancestors)

        return Tree(root=root)

    def delete_fixup(self, ancestors):
        ancestors = ancestors[:]
        node = ancestors.pop()

        def replace_top(new_top):
            old_top = ancestors.pop()
            if ancestors:
                ancestors[-1].replace_child(new_top, old_top)

            ancestors.append(new_top)

        while ancestors and isBlackOrNil(node):
            if node:
                direction = ancestors[-1].child_direction(node)
            elif ancestors[-1].left:
                direction = 'right'
            else:
                direction = 'left'

            sibling = ancestors[-1].other(direction)

            if sibling and sibling.isRed():
                new_top = ancestors[-1].rotate(direction)
                new_top.color = Color.BLACK
                new_top.child(direction).color = Color.RED

                replace_top(new_top)
                ancestors.append(new_top.child(direction))
                node = new_top.child(direction).child(direction)
                sibling = new_top.child(direction).other(direction)

            if isBlackOrNil(sibling.left) and isBlackOrNil(sibling.right):
                ancestors[-1].replace_child(sibling.copy(color=Color.RED), sibling)
                node = ancestors.pop()
            else:
                if isBlackOrNil(sibling.other(direction)):
                    new_sibling = sibling.rotate(other(direction))
                    new_sibling.color = Color.BLACK
                    new_sibling.other(direction).color = Color.RED

                    ancestors[-1].replace_child(new_sibling, sibling)
                    sibling = new_sibling

                new_top = ancestors[-1].rotate(direction)
                new_top.color = ancestors[-1].color
                new_top.child(direction).color = Color.BLACK
                new_top.replace_child(new_top.other(direction).copy(color=Color.BLACK), new_top.other(direction))
                node = None
                replace_top(new_top)
                break

        if node:
            node.color = Color.BLACK

        return ancestors[0] if ancestors else node
