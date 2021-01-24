#include "02.c"
#include "../../build/ext/test.h"

#include <stdlib.h>

tree_t *s(int key, tree_t *left, tree_t *right) {
    tree_t *new = malloc(sizeof(tree_t));
    new->parent = NULL;

    new->key = key;
    new->left = left;
    new->right = right;

    if (left) left->parent = new;
    if (right) right->parent = new;

    return new;
}

TEST(minumum) {
    tree_t *tree =
        s(15, s(6, s(3, s(2, NULL, NULL),
                        s(4, NULL, NULL)),
                    s(7, NULL,
                         s(13, s(9, NULL, NULL),
                               NULL))),
              s(18, s(17, NULL, NULL),
                    s(20, NULL, NULL)));

    ASSERT_EQUALS(minimum(tree)->key, 2);
}


TEST(maximum) {
    tree_t *tree =
        s(15, s(6, s(3, s(2, NULL, NULL),
                        s(4, NULL, NULL)),
                    s(7, NULL,
                         s(13, s(9, NULL, NULL),
                               NULL))),
              s(18, s(17, NULL, NULL),
                    s(20, NULL, NULL)));

    ASSERT_EQUALS(maximum(tree)->key, 20);
}
