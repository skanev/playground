import os.path as path
import random
import time

filename = path.join(path.dirname(__file__), '01.py')
exec(open(filename).read())

def report_time(name, func):
    begin = time.time()
    func()
    end = time.time()
    print("{:} = {:.4f}s".format(name, end - begin))

array = []
random.seed(300)
for _ in range(10000):
    array.append(random.randint(0, 999))

report_time('merge-sort', lambda: merge_sort(array[:], 0, len(array) - 1))
report_time('mixed-sort', lambda: mixed_sort(array[:], 0, len(array) - 1))
