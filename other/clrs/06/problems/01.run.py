import os.path as path
import random
import time

filename = path.join(path.dirname(__file__), '01.py')
exec(open(filename).read())

data  = [1, 2, 3, 4, 5, 6]
funcs = [("BUILD-MAX-HEAP: ", build_max_heap), ("BUILD-MAX-HEAP':", build_max_heap2)]

print('Heap builds for: {}'.format(', '.join(map(str, data))))
print('---------------------------------')

for (label, func) in funcs:
    h = heap(data[:])
    func(h)
    print('{} {}'.format(label, ', '.join(str(item) for item in h.items)))
