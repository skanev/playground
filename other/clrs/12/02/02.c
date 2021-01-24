struct tree_t {
    struct tree_t *left;
    struct tree_t *right;
    struct tree_t *parent;
    int key;
};
typedef struct tree_t tree_t;

tree_t *minimum(tree_t *tree) {
    return tree->left ? minimum(tree->left) : tree;
}

tree_t *maximum(tree_t *tree) {
    return tree->right ? maximum(tree->right) : tree;
}

