from queue import PriorityQueue


class HuffmanNode:
    def __init__(self, name, freq, left, right):
        self.name = name
        self.freq = freq
        self.left = left
        self.right = right

    def __lt__(self, other):
        return (self.freq, isinstance(self, InternalNode)) < (other.freq, isinstance(other, InternalNode))


class TerminalNode(HuffmanNode):
    def __init__(self, name, freq):
        super().__init__(name, freq, None, None)


class InternalNode(HuffmanNode):
    def __init__(self, left, right):
        super().__init__(None, left.freq + right.freq, left, right)


def huffman(freqs):
    queue = PriorityQueue()

    for char, freq in freqs.items():
        queue.put(TerminalNode(char, freq))

    for _ in range(len(freqs) - 1):
        left = queue.get()
        right = queue.get()
        queue.put(InternalNode(left, right))

    return queue.get()
