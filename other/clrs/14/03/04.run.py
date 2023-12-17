solution = __import__('04')

example = [10, 4, 3, 2]


def report(message, answers):
    multiplications, order = answers

    print(f"{message}: {order} with {multiplications} scalar multiplications")


report("wise decision", solution.order(example))
report("poor decision", solution.greedy(example))
