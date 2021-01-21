import os.path as path

filename = path.join(path.dirname(__file__), '01.py')
exec(open(filename).read())


keys = [10, 22, 31, 4, 15, 28, 17, 88, 59]
m = 11
probes = [
    ('linear', linear(m)),
    ('quadratic', quadratic(m, 1, 3)),
    ('double', double(m)),
]

for (name, probe) in probes:
    table = " | ".join(("{:>2}".format(n) if n else "  " for n in populate(m, keys, probe)))
    print("{:<10}:  {}".format(name, table))
