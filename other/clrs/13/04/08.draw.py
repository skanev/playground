import os.path as path
import drawing

exec(open(path.join(path.dirname(__file__), '03.py')).read())

B = drawing.RedBlackTrees.Black
R = drawing.RedBlackTrees.Red


def dot(tree):
    def convert(node, nil):
        if node is nil:
            return None

        cons = R if node.color == Color.RED else B
        left = convert(node.left, nil)
        right = convert(node.right, nil)

        return cons(node.key, left, right)

    return convert(tree.root, tree.nil).dot()


tree = Tree()
drawings = []
tree.insert(5)
tree.insert(4)
tree.insert(6)

drawings.append({'name': f'Before insert', 'dot': dot(tree), 'display': True})

tree.insert(3)

drawings.append({'name': f'After insert', 'dot': dot(tree), 'display': True})

tree.delete(3)

drawings.append({'name': f'After delete', 'dot': dot(tree), 'display': True})

drawing.process(drawings)
