#include <stdlib.h>

struct node_t {
    struct node_t *left;
    struct node_t *right;
    struct node_t *succ;
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

    node->succ = NULL;
    node->left = NULL;
    node->right = NULL;
    node->key = key;

    return node;
}

node_t *insert(tree_t *tree, int key) {
    node_t *parent = NULL;
    node_t *current = tree->root;
    node_t *new = make_node(key);
    node_t *pred = NULL;

    while (current) {
        parent = current;
        if (new->key < current->key) {
            new->succ = current;
            current = current->left;
        } else {
            pred = current;
            current = current->right;
        }
    }

    if (!parent) {
        tree->root = new;
    } else if (new->key < parent->key) {
        parent->left = new;
    } else {
        parent->right = new;
        pred = parent;
    }

    if (pred) pred->succ = new;

    return new;
}

node_t *find_parent(tree_t *tree, node_t *node) {
    node_t *previous = NULL;
    node_t *current = tree->root;

    while (current && current->key != node->key) {
        if (current->key < node->key) {
            previous = current;
            current = current->right;
        } else {
            previous = current;
            current = current->left;
        }
    }

    return previous;
}

void find_parent_and_predecessor(tree_t *tree, node_t *node, node_t **parent, node_t **predecessor) {
    *parent = NULL;
    *predecessor = NULL;
    node_t *current = tree->root;

    while (current && current->key != node->key) {
        if (current->key < node->key) {
            *parent = current;
            *predecessor = current;
            current = current->right;
        } else {
            *parent = current;
            current = current->left;
        }
    }

    if (!current) return;

    current = current->left;

    while (current) {
        *predecessor = current;
        current = current->right;
    }
}

void transplant(tree_t *tree, node_t *parent, node_t *target, node_t *source) {
    if (!parent) {
        tree->root = source;
    } else if (target == parent->left) {
        parent->left = source;
    } else {
        parent->right = source;
    }
}

void delete_tree(tree_t *tree, node_t *node) {
    node_t *parent;
    node_t *predecessor;

    find_parent_and_predecessor(tree, node, &parent, &predecessor);

    if (!node->left) {
        transplant(tree, parent, node, node->right);
    } else if (!node->right) {
        transplant(tree, parent, node, node->left);
    } else {
        node_t *successor = node->succ;

        if (node->right != successor) {
            node_t *sparent = find_parent(tree, successor);
            transplant(tree, sparent, successor, successor->right);
            successor->right = node->right;
        }

        transplant(tree, parent, node, successor);
        successor->left = node->left;
    }

    if (predecessor) predecessor->succ = node->succ;
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

typedef void (*callback_t)(node_t *);

void inorder_walk(node_t *node, callback_t callback) {
    if (!node) return;
    inorder_walk(node->left, callback);
    callback(node);
    inorder_walk(node->right, callback);
}

void successor_walk(node_t *node, callback_t callback) {
    while (node->left) node = node->left;

    while (node) {
        callback(node);
        node = node->succ;
    }
}
