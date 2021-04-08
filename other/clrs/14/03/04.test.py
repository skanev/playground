import unittest
import random
import os.path as path


filename = path.join(path.dirname(__file__), '04.py')
exec(open(filename).read())


class IntervalTreeTest(unittest.TestCase):
    def test_search_all(self):
        tree = IntervalTree()
        three_to_five = Interval(3, 5)
        seven_to_nine = Interval(7, 9)
        eleven_to_thirteen = Interval(11, 13)

        tree.insert(three_to_five)
        tree.insert(seven_to_nine)
        tree.insert(eleven_to_thirteen)


        self.assertEqual(set(tree.search_all(Interval(3, 8))),
                         {three_to_five, seven_to_nine})

        self.assertEqual(set(tree.search_all(Interval(7, 10))),
                         {seven_to_nine})

        self.assertEqual(set(tree.search_all(Interval(7, 12))),
                         {seven_to_nine, eleven_to_thirteen})

        self.assertEqual(set(tree.search_all(Interval(1, 15))),
                         {three_to_five, seven_to_nine, eleven_to_thirteen})


if __name__ == '__main__':
    unittest.main()
