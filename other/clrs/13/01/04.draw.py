import drawing

B = drawing.RedBlackTrees.Black
R = drawing.RedBlackTrees.Red

tree = B(4,
        R(2, B(1), B(3)),
        R(6, B(5), B(7)))

drawings = [
    {'name': 'Max degree', 'dot': tree.dot(nils=False), 'display': False},
]

drawing.process(drawings)
