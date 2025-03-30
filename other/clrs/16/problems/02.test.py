import unittest
import random

DBS = __import__('02').DBS

class DynamicBinarySearchTest(unittest.TestCase):
    def test_random_sequence_of_operations(self):
        random.seed(0)
        max = 10000
        n = 400
        deletions = 100

        free_insertions = n - deletions * 2

        numbers = random.sample(list(range(1, max + 1)), n)
        inserts = [('insert', n) for n in numbers[deletions:]]
        deletes = [('delete', n) for n in random.sample(numbers[:deletions], deletions) + list(range(max, max + 10))]
        order = inserts + deletes
        random.shuffle(order)

        dbs = DBS()

        def check():
            for n in numbers:
                self.assertEqual(n in baseline, n in dbs)

        baseline = set()
        for i in range(deletions):
            baseline.add(numbers[i])
            dbs.insert(numbers[i])

            check()

        for (op, n) in order:
            match op:
                case 'insert':
                    baseline.add(n)
                    dbs.insert(n)
                case 'delete':
                    baseline.discard(n)
                    dbs.delete(n)

            check()


if __name__ == '__main__':
    unittest.main()
