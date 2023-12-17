#include "06.c"
#include <stdio.h>

#define W 6
#define P 2
#define MAX(a,b) (((a)>(b))?(a):(b))

int height(node_t *tree) {
    if (!tree) return 0;

    return MAX(height(tree->left), height(tree->right)) + 1;
}

node_t **make_array(int size) {
    return calloc(sizeof(node_t *), size);
}

int size(node_t *node) {
    if (!node) return 0;
    return size(node->left) + size(node->right) + 1;
}

void space(int i) {
    while(i-- > 0) putchar(' ');
}

int level_pad(int h) {
    int n = 1 << (h - 1);
    return (n - 1) * (W + P) / 2;
}

void print_node(node_t *tree) {
    if (!tree) printf("      ");
    else if (tree->succ) printf("%2d(%2d)", tree->key, tree->succ->key);
    else printf("%2d(--)", tree->key);
}

void print_tree(tree_t *tree) {
    int h = height(tree->root);

    int width = 1;
    node_t **row = make_array(width);
    row[0] = tree->root;

    for (int level = 0; level < h; level++) {
        int padding = level_pad(h - level);
        for (int i = 0; i < width; i++) {
            space(padding);
            print_node(row[i]);
            space(padding);
            space(P);
        }
        puts("");
        puts("");

        node_t **next = make_array(width * 2);

        for (int c = 0, t = 0; c < width; c++) {
            next[t++] = row[c] ? row[c]->left : NULL;
            next[t++] = row[c] ? row[c]->right : NULL;
        }

        free(row);
        width *= 2;
        row = next;
    }

    free(row);
}
