import drawing

B = drawing.RedBlackTrees.Black
R = drawing.RedBlackTrees.Red
G = drawing.RedBlackTrees.Gray

tree = B(
    26,
    R(17,
        B(14,
            R(10,
                B(7, R(3)),
                B(12)),
            B(16, R(15))),
        B(21,
            B(19, None, R(20)),
            B(23))),
    B(41,
        R(30,
            B(28),
            B(38,
                R(35, None, G(36)),
                R(39))),
        B(47)))

drawings = [
    {'name': 'Figure 13.1', 'dot': tree.dot(), 'display': False},
]

drawing.process(drawings)
