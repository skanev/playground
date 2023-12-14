struct tree_t {
    struct tree_t *left;
    struct tree_t *right;
    struct tree_t *parent;
    int key;
};
typedef struct tree_t tree_t;
void store(int);

void print_tree(tree_t *tree) {
    store(tree->key);

    if (tree->left)
        print_tree(tree->left);

    if (tree->right)
        print_tree(tree->right);
}

#define MAX_SIZE 10
int keys[MAX_SIZE];
int count = 0;

void reset_storage() {
    count = 0;
}

void store(int key) {
    keys[count++] = key;
}
