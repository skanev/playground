#include <stdlib.h>
#include <stdio.h>

struct node_t {
    struct node_t *zero;
    struct node_t *one;
    const char *str;
};

typedef struct node_t node_t;

typedef struct {
    node_t *root;
} radix_tree_t;

typedef void (*callback_t)(const char *);

node_t *make_node() {
    node_t *new = malloc(sizeof(node_t));
    new->zero = NULL;
    new->one = NULL;
    new->str = NULL;
    return new;
}

radix_tree_t *make_radix_tree() {
    radix_tree_t *new = malloc(sizeof(radix_tree_t));
    new->root = make_node();
    return new;
}

void insert(radix_tree_t *tree, const char *string) {
    const char *s = string;
    node_t *current = tree->root;

    while (*s != '\0') {
        if (*s == '0') {
            if (!current->zero) current->zero = make_node();
            current = current->zero;
        } else if (*s == '1') {
            if (!current->one) current->one = make_node();
            current = current->one;
        } else {
            fprintf(stderr, "Invalid string: %s", s);
            exit(1);
        }
        s++;
    }

    current->str = string;
}

void walk(node_t *node, callback_t callback) {
    if (node->str) callback(node->str);
    if (node->zero) walk(node->zero, callback);
    if (node->one) walk(node->one, callback);
}

void walk_sorted(radix_tree_t *tree, callback_t callback) {
    walk(tree->root, callback);
}
