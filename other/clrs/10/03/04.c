#define MAX_SIZE 10

struct tree_t {
    struct tree_t *child;
    struct tree_t *sibling;
    struct tree_t *parent;
    int key;
};
typedef struct tree_t tree_t;
void store(int);

void print_tree(tree_t *tree) {
    store(tree->key);

    if (tree->child)
        print_tree(tree->child);

    if (tree->sibling)
        print_tree(tree->sibling);
}

int keys[MAX_SIZE];
int count = 0;

void reset_storage() {
    count = 0;
}

void store(int key) {
    keys[count++] = key;
}
