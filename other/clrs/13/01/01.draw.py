import drawing

B = drawing.RedBlackTrees.Black
R = drawing.RedBlackTrees.Red

tree01 = B(8,
            R(4,
                B(2, R(1), R(3)),
                B(6, R(5), R(7))),
            R(12,
                B(10, R(9), R(11)),
                B(14, R(13), R(15)))).dot()

tree02 = B(8,
            B(4,
                R(2, B(1), B(3)),
                R(6, B(5), B(7))),
            B(12,
                R(10, B(9), B(11)),
                R(14, B(13), B(15)))).dot()


tree03 = B(8,
            B(4,
                B(2, B(1), B(3)),
                B(6, B(5), B(7))),
            B(12,
                B(10, B(9), B(11)),
                B(14, B(13), B(15)))).dot()

drawings = [
    {'name': 'Black-height 2', 'dot': tree01, 'display': False},
    {'name': 'Black-height 3', 'dot': tree02, 'display': False},
    {'name': 'Black-height 4', 'dot': tree03, 'display': False},
]

drawing.process(drawings)
