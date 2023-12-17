module = __import__('01')

p = [None, 0.15, 0.10, 0.05, 0.10, 0.20]
q = [0.05, 0.10, 0.05, 0.05, 0.05, 0.10]

a, root = module.optimal(p, q)

module.dump_tree(root)
