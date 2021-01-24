struct tree_t {
    struct tree_t *left;
    struct tree_t *right;
    struct tree_t *parent;
    int key;
};
typedef struct tree_t tree_t;

tree_t *maximum(tree_t *tree) {
    while (tree->right) tree = tree->right;
    return tree;
}

tree_t *predecessor(tree_t *tree) {
    if (tree->left) {
        return maximum(tree->left);
    }

    tree_t *parent = tree->parent;

    while (parent && parent->left == tree) {
        tree = tree->parent;
        parent = tree->parent;
    }

    return parent;
}
