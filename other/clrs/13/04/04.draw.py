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

for n in [41, 38, 31, 12, 19, 8]:
    tree.insert(n)

for n in [8, 12, 19, 31, 38]:
    tree.delete(n)
    drawings.append({'name': f'After deleting {n}', 'dot': dot(tree), 'display': True})

drawing.process(drawings)
