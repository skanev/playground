import unittest
import random
import functools

knapsack = __import__("02").knapsack

def generate(n, value_spec, weight_spec):
    vmin, vmax, vstep, vcut = value_spec
    wmin, wmax = weight_spec

    result = []

    for _ in range(n):
        value = random.randint(vmin // vstep, vmax // vstep) * vcut + random.randint(1, vcut)
        weight = random.randint(vmin, vmax)

        result.append((value, weight))

    return result

def naive(limit, items):
    @functools.cache
    def solve(gained, used, taken):
        possible = [(gained, used, taken)]

        for (i, (value, weight)) in enumerate(items):
            if taken[i] or used + weight > limit:
                continue

            possible.append(solve(gained + value, used + weight, taken[:i] + (True,) + taken[i+1:]))

        return max(possible, key=lambda x: x[0])

    gained, used, taken = solve(0, 0, tuple(False for _ in items))
    return (gained, used, [items[i] for (i, use) in enumerate(taken) if use])


class KnapsackTest(unittest.TestCase):
    def test_random_samples(self):
        for i in range(10):
            problem = generate(20, (40, 100, 20, 3), (10, 50))
            expected, _, _ = naive(400, problem)
            actual, _, _ = knapsack(400, problem)

            self.assertEqual(expected, actual)


if __name__ == "__main__":
    unittest.main()
