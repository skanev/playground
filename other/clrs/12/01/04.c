struct tree_t {
    struct tree_t *left;
    struct tree_t *right;
    int key;
};
typedef struct tree_t tree_t;

typedef void callback_t(tree_t *node);

void preorder(tree_t *tree, callback_t *callback) {
    if (!tree) return;

    callback(tree);
    preorder(tree->left, callback);
    preorder(tree->right, callback);
}

void postorder(tree_t *tree, callback_t *callback) {
    if (!tree) return;

    postorder(tree->left, callback);
    postorder(tree->right, callback);
    callback(tree);
}
