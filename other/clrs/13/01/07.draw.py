import drawing

B = drawing.RedBlackTrees.Black
R = drawing.RedBlackTrees.Red

tree = B(8,
        R(4,
            B(2, R(1), R(3)),
            B(6, R(5), R(7))),
        R(12,
            B(10, R(9), R(11)),
            B(14, R(13), R(15))))

drawings = [
    {'name': 'Biggest ratio', 'dot': tree.dot(nils=False), 'display': False},
]

drawing.process(drawings)
