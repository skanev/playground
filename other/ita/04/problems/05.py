import random

class GoodChip:
    def good(self):
        return True

    def check(self, other):
        return other.good()

class BadChip:
    def good(self):
        return False

    def check(self, other):
        return [True, False][random.randint(0, 1)]

def jig(a, b):
    return [a.check(b), b.check(a)]

def diogenes(chips, verbose = False):
    def find_single(chips):
        if len(chips) <= 2:
            return chips[0]
        else:
            halfpoint = len(chips) // 2
            pairs     = zip(chips[0:halfpoint], chips[halfpoint:halfpoint * 2])
            kept      = [a for (a, b) in pairs if jig(a, b) == [True, True]]

            if len(chips) % 2 == 1:
                kept.append(chips[-1])

            return find_single(kept)

    good = find_single(chips)
    return [chip for chip in chips if jig(good, chip) == [True, True]]
