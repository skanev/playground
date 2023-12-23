import unittest
import random
import functools

from collections import namedtuple

best_party = __import__('06').best_party

id = 0

def next_id():
    global id
    id += 1
    return id

def generate_hierarchy(depth = 4, width = 4, seed = 20):
    random.seed(seed)

    def generate_tree(depth):
        tree = {'id': next_id(), 'conviviality': random.randint(1, 30), 'children': []}

        if depth > 0:
            for _ in range(random.randint(1, width)):
                tree['children'].append(generate_tree(depth - 1))

        return tree

    return generate_tree(depth)


def naive(hierarchy):
    parents = {}
    nodes = {}
    ids = []

    def walk(tree, parent_id):
        nodes[tree['id']] = tree
        parents[tree['id']] = parent_id
        ids.append(tree['id'])

        for child in tree['children']:
            walk(child, tree['id'])


    def options(invited, first):
        if first == len(ids):
            yield invited
            return

        next = ids[first]

        if parents[next] not in invited:
            yield from options(invited | {next}, first + 1)

        yield from options(invited, first + 1)

    walk(hierarchy, None)

    best = 0

    for people in options(set(), 0):
        conviviality = sum(nodes[id]['conviviality'] for id in people)
        assert not any(parents[id] in people for id in people)
        best = max(best, conviviality)

    return best

Node = namedtuple('Node', ['id', 'conviviality', 'left_child', 'right_sibling'])

def desired_format(hierarchy):
    def walk(tree, siblings):
        left_child = walk(tree['children'][0], tree['children'][1:]) if tree['children'] else None
        right_sibling = walk(siblings[0], siblings[1:]) if siblings else None

        return Node(tree['id'], tree['conviviality'], left_child, right_sibling)

    return walk(hierarchy, [])


class PartyPlanningTest(unittest.TestCase):
    def test_party_planning(self):
        random.seed(0)
        hierarchy = generate_hierarchy()
        node = desired_format(hierarchy)
        expected = naive(hierarchy)

        self.assertEqual(best_party(node), expected)

if __name__ == '__main__':
    unittest.main()
