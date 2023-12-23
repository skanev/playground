def best_party(root):
    def best(node):
        included = node.conviviality
        excluded = 0

        child = node.left_child
        while child:
            child_present, child_absent = best(child)
            included += child_absent
            excluded += max(child_present, child_absent)
            child = child.right_sibling

        return (included, excluded)

    return max(best(root))
