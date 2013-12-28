#define MAX_SIZE 10

struct tree_t {
    struct tree_t *left;
    struct tree_t *right;
    struct tree_t *parent;
    int key;
};
typedef struct tree_t tree_t;
void store(int);

void print_tree(tree_t *tree) {
    tree_t *stack[MAX_SIZE];
    int count = 0;

    stack[count++] = tree;

    while (count) {
        tree = stack[--count];

        store(tree->key);

        if (tree->right)
            stack[count++] = tree->right;

        if (tree->left)
            stack[count++] = tree->left;
    }
}

int keys[MAX_SIZE];
int count = 0;

void reset_storage() {
    count = 0;
}

void store(int key) {
    keys[count++] = key;
}
