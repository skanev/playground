import sys
import re
from collections import deque
from subprocess import Popen, PIPE, STDOUT

unique = 0


def unique_number():
    global unique
    unique += 1
    return unique


class RedBlackTrees:
    class Node:
        def __init__(self, value, left=None, right=None, label=None, extra=[]):
            self.id = unique_number()
            self.value = value
            self.label = label or value
            self.left = left
            self.right = right
            self.extra = extra

        def dot(self, nils=True):
            def nil_node(n):
                return f'nil{n}[shape=point];'

            nodes = []
            edges = []

            lines = []
            lines.append("graph {")
            lines.append("  node[shape=circle, style=filled];")
            lines.append("")

            nodes.append(self.node())

            for item in self.bfs():
                if item.left:
                    nodes.append(item.left.node())
                    edges.append(f"{item.name()} -- {item.left.name()};")
                elif nils:
                    n = unique_number()
                    nodes.append(nil_node(n))
                    edges.append(f"{item.name()} -- nil{n};")

                if item.right:
                    nodes.append(item.right.node())
                    edges.append(f"{item.name()} -- {item.right.name()};")
                elif nils:
                    n = unique_number()
                    nodes.append(nil_node(n))
                    edges.append(f"{item.name()} -- nil{n};")

            for node in nodes:
                lines.append(f"  {node}")

            lines.append("")

            for edge in edges:
                lines.append(f"  {edge}")

            lines.append("}")
            lines.append("")

            return "\n".join(lines)

        def name(self):
            return f"n{self.id}"

        def node(self):
            attrs = ", ".join([f"label=\"{self.label}\"",
                               *self.attributes(),
                               *self.extra])
            return f"{self.name()}[{attrs}];"

        def bfs(self):
            queue = deque()
            queue.append(self)

            while queue:
                item = queue.popleft()
                yield item
                if item.left:
                    queue.append(item.left)
                if item.right:
                    queue.append(item.right)

    class Red(Node):
        def attributes(self):
            return ['fillcolor=red', 'fontcolor=white']

    class Black(Node):
        def attributes(self):
            return ['fillcolor=black', 'fontcolor=white']

    class Gray(Node):
        def attributes(self):
            return ['fillcolor=gray', 'fontcolor=white']


def svg(dot):
    with Popen(['dot', '-Tsvg'], stdin=PIPE, stdout=PIPE, stderr=STDOUT) as p:
        output = p.communicate(bytes(dot, 'utf-8'))[0]
        return output.decode()


def process(drawings):
    command = sys.argv[1]
    if command == 'list':
        for (i, drawing) in enumerate(drawings):
            number = '{:02d}'.format(i + 1)
            name = re.sub('^.*?(\d+)/(\d+|problems)/(\d+)\.draw.py', f'\\1/\\2/\\3.drawing.{number}.svg', sys.argv[0])
            print("{} {} {} {}".format(number, name, 'true' if drawing['display'] else 'false', drawing['name']))
    elif command == 'draw':
        index = int(sys.argv[2]) - 1
        dot = drawings[index]['dot']
        print(svg(dot))
    elif command == 'debug':
        index = int(sys.argv[2]) - 1
        dot = drawings[index]['dot']
        print(dot)
    else:
        raise f"Uknown commands: {repr(sys.argv)}"
