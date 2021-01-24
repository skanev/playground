#include "04.c"
#include "../../build/ext/test.h"

#include <stdlib.h>

#define MAX_SIZE 10
int keys[MAX_SIZE];
int count = 0;

void reset_storage() {
    count = 0;
}

void store(tree_t *tree) {
    keys[count++] = tree->key;
}

tree_t *s(int key, tree_t *left, tree_t *right) {
    tree_t *new = malloc(sizeof(tree_t));
    new->key = key;
    new->left = left;
    new->right = right;
    return new;
}

TEST(preorder_walk) {
    tree_t *tree =
        s(10, s(6, s(4, s(1, NULL, NULL),
                        s(5, NULL, NULL)),
                    NULL),
              s(16, s(15, NULL, NULL),
                    s(20, s(19, NULL, NULL),
                          NULL)));

    reset_storage();

    preorder(tree, store);

    int expected[] = {10, 6, 4, 1, 5, 16, 15, 20, 19};

    ASSERT_SAME_ARRAYS_S(keys, expected, 9);
}

TEST(postorder_walk) {
    tree_t *tree =
        s(10, s(6, s(4, s(1, NULL, NULL),
                        s(5, NULL, NULL)),
                    NULL),
              s(16, s(15, NULL, NULL),
                    s(20, s(19, NULL, NULL),
                          NULL)));

    reset_storage();

    postorder(tree, store);

    int expected[] = {1, 5, 4, 6, 15, 19, 20, 16, 10};

    ASSERT_SAME_ARRAYS_S(keys, expected, 9);
}

