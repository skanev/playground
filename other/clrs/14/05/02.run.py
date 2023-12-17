module = __import__('02')

p = [None, 0.04, 0.06, 0.08, 0.02, 0.10, 0.12, 0.14]
q = [0.06, 0.06, 0.06, 0.06, 0.05, 0.05, 0.05, 0.05]

_, root = module.optimal(p, q)

module.dump_tree(root)
