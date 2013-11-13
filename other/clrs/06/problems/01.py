##############################################################################
# DATA STRUCTURES
##############################################################################

class heap:
    def __init__(self, items, size = None):
        self.items = items
        self.heap_size = size or len(items)

    def __getitem__(self, key):
        return self.items[key]

    def __setitem__(self, key, value):
        self.items[key] = value

    def __len__(self):
        return len(self.items)

def left(i):
    return 2 * i + 1

def right(i):
    return 2 * i + 2

def parent(i):
    return (i - 1) // 2

##############################################################################
# Standard BUILD-MAX-HEAP
##############################################################################

def max_heapify(A, i):
    l = left(i)
    r = right(i)
    if l < A.heap_size and A[l] > A[i]:
        largest = l
    else:
        largest = i
    if r < A.heap_size and A[r] > A[largest]:
        largest = r

    if largest != i:
        A[i], A[largest] = A[largest], A[i]
        max_heapify(A, largest)

def build_max_heap(A):
    A.heap_size = len(A)
    for i in range(len(A) // 2, -1, -1):
        max_heapify(A, i)

##############################################################################
# Exercise BUILD-MAX-HEAP'
##############################################################################

def heap_increase_key(A, i, key):
    if key < A[i]:
        raise Exception("new key is smaller than current key")
    A[i] = key
    while i > 0 and A[parent(i)] < A[i]:
        A[i], A[parent(i)] = A[parent(i)], A[i]
        i = parent(i)

def max_heap_insert(A, key):
    A.heap_size += 1
    A[A.heap_size - 1] = float("-inf")
    heap_increase_key(A, A.heap_size - 1, key)

def build_max_heap2(A):
    A.heap_size = 1
    for i in range(1, len(A)):
        max_heap_insert(A, A[i])
