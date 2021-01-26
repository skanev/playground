#include <stdlib.h>

struct node_t {
    struct node_t *parent;
    struct node_t *left;
    struct node_t *right;
    int key;
};
typedef struct node_t node_t;

typedef struct {
    node_t *root;
} tree_t;

tree_t *make_tree() {
    tree_t *tree = malloc(sizeof(tree_t));
    tree->root = NULL;
    return tree;
}

node_t *make_node(int key) {
    node_t *node = malloc(sizeof(node_t));

    node->parent = NULL;
    node->left = NULL;
    node->right = NULL;
    node->key = key;

    return node;
}

node_t *insert_node(node_t *node, int key) {
    if (node->key < key) {
        if (node->right) {
            return insert_node(node->right, key);
        } else {
            node_t *new = make_node(key);
            new->parent = node;
            node->right = new;
            return new;
        }
    } else {
        if (node->left) {
            return insert_node(node->left, key);
        } else {
            node_t *new = make_node(key);
            new->parent = node;
            node->left = new;
            return new;
        }
    }
}

node_t *insert(tree_t *tree, int key) {
    if (tree->root) {
        return insert_node(tree->root, key);
    } else {
        node_t *node = make_node(key);
        tree->root = node;
        return node;
    }
}

node_t *search(tree_t *tree, int key) {
    node_t *node = tree->root;

    while (node) {
        if (node->key == key) {
            return node;
        } else if (node->key < key) {
            node = node->right;
        } else {
            node = node->left;
        }
    }

    return NULL;
}
