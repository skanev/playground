import os.path as path
filename = path.join(path.dirname(__file__), '05.py')
exec(open(filename).read())
