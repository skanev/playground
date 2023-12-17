import unittest
import os.path as path
import random

filename = path.join(path.dirname(__file__), '08.py')
exec(open(filename).read())


def naive_chord_count(chords):
    count = 0

    for i in range(len(chords)):
        for j in range(i + 1, len(chords)):
            (ia, ib) = chords[i]
            (ja, jb) = chords[j]

            if ib < ia:
                ia, ib = ib, ia

            if jb < ja:
                ja, jb = jb, ja

            if ia <= ja <= ib <= jb or ja <= ia <= jb <= ib:
                count += 1

    return count


class OrderStatisticTreeTest(unittest.TestCase):
    def test_diameters(self):
        def generate_diameters(n):
            chords = None

            while True:
                chords = []
                seen = set()

                for i in range(n):
                    start = random.random()
                    end = 0.5 + start
                    if end >= 1:
                        end -= 1.0

                    seen.add(start)
                    seen.add(end)

                    chords.append((start, end))

                if len(seen) == n * 2:
                    break

            return chords

        n = 50
        chords = generate_diameters(n)

        self.assertEqual(naive_chord_count(chords), (n * (n - 1)) / 2)
        self.assertEqual(count_chords(chords), (n * (n - 1)) / 2)

    def test_random_circle(self):
        def generate_chords(n):
            chords = None

            while True:
                chords = []
                seen = set()

                for i in range(n):
                    start = random.random()
                    end = random.random()

                    seen.add(start)
                    seen.add(end)

                    chords.append((start, end))

                if len(seen) == n * 2:
                    break

            return chords

        n = 100
        for _ in range(100):
            chords = generate_chords(n)
            self.assertEqual(count_chords(chords), naive_chord_count(chords))


if __name__ == '__main__':
    unittest.main()
