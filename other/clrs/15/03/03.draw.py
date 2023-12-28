from os import path
from collections import deque
import drawing

exec(open(path.join(path.dirname(__file__), '03.py')).read())


def dot(node):
    order = []

    left = deque()
    left.append(node)

    while left:
        item = left.popleft()
        order.append(item)

        if item.left:
            left.append(item.left)
        if item.right:
            left.append(item.right)

    lines = ["digraph {"]
    for n in order:
        node_id = f"n{id(n)}"
        if isinstance(n, TerminalNode):
            lines.append(f'{node_id}[label="{n.name}:{n.freq}", shape=square];')
        else:
            lines.append(f"{node_id}[label={n.freq}];")

    for n in order:
        if isinstance(n, InternalNode):
            lines.append(f"n{id(n)} -> n{id(n.left)} [label=0];")
            lines.append(f"n{id(n)} -> n{id(n.right)} [label=1];")

    lines.append("}")
    return "\n".join(lines)


chapter_example = huffman({"a": 45, "b": 13, "c": 12, "d": 16, "e": 9, "f": 5})
exercise = huffman({'a': 1, 'b': 1, 'c': 2, 'd': 3, 'e': 5, 'f': 8, 'g': 13, 'h': 21})

drawing.process([
    {'name': 'Chapter Example', 'dot': dot(chapter_example), 'display': True},
    {'name': 'Fibonnaci code', 'dot': dot(exercise), 'display': True},
])
