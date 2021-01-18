import os.path as path

filename = path.join(path.dirname(__file__), '04.py')
exec(open(filename).read())

keys = [61, 62, 63, 64, 65]


for key in keys:
    print("h({}) = {}".format(key, h(key)))
