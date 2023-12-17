from augmentable_tree import AugmentableTree

def node_size(node):
    return node.size if node else 0

def select_node(node, i):
    while node:
        rank = node_size(node.left) + 1
        if i == rank:
            return node
        elif i < rank:
            node = node.left
        else:
            i -= rank
            node = node.right

    assert(False)

def rank_node(node):
    rank = node_size(node.left) + 1

    while node.parent:
        if node == node.parent.right:
            rank += node_size(node.parent.left) + 1
        node = node.parent

    return rank


class OrderStatisticTree(AugmentableTree):
    def augment_node(self, node):
        node.rank = lambda: rank_node(node)
        node.select = lambda i: select_node(node, i)
        node.size = 1

    def recalculate_node(self, node):
        node.size = 1 + node_size(node.left) + node_size(node.right)

    def select(self, i):
        return select_node(self.root, i)
