import unittest
import random
import os.path as path
import random
import time

filename = path.join(path.dirname(__file__), '05.py')
exec(open(filename).read())

random.seed(1)

def generate_chips(count):
    good = [GoodChip() for _ in range(random.randint(count // 2 + 1, 3 * count // 4 + 1))]
    bad  = [BadChip() for _ in range(count - len(good))]
    chips = good + bad
    random.shuffle(chips)

    return chips

class DiogenesTest(unittest.TestCase):
    def test_correctness(self):
        for n in range(100):
            chips = generate_chips(random.randint(1, 1000))
            good = diogenes(chips, n == 155)
            good_count = sum(1 for chip in chips if chip.good())
            self.assertTrue(good_count == len(good))
            self.assertTrue(all(chip.good for chip in good))

if __name__ == '__main__':
    unittest.main()
