import unittest
import random

module = __import__("01")
top_down, bottom_down = module.top_down, module.bottom_down

chapter_example = [
    (1, 4),
    (3, 5),
    (0, 6),
    (5, 7),
    (3, 9),
    (5, 9),
    (6, 10),
    (7, 11),
    (8, 12),
    (2, 14),
    (12, 16),
]


def generate(span, count, max):
    activities = []
    for i in range(count):
        start = random.randint(0, span - max)
        activities.append((start, start + random.randint(1, max)))

    return activities


def greedy(activities):
    activities = sorted(activities, key=lambda x: x[1])

    answer = [activities[0]]
    k = 0
    for m in range(1, len(activities)):
        if activities[m][0] >= activities[k][1]:
            answer.append(activities[m])
            k = m

    return answer


class DynamicProgrammingTest(unittest.TestCase):
    def test_chapter_example(self):
        self.assertEqual(greedy(chapter_example), [(1, 4), (5, 7), (7, 11), (12, 16)])
        self.assertEqual(top_down(chapter_example), len(greedy(chapter_example)))

    def test_random_samples(self):
        for i in range(10):
            sample = generate(1000, 100, 50)
            self.assertEqual(top_down(sample), len(greedy(sample)))
            self.assertEqual(bottom_down(sample), len(greedy(sample)))


if __name__ == "__main__":
    unittest.main()
