#include "03.c"
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

TEST(predecessor) {
    tree_t *t13;
    tree_t *t17;
    tree_t *t9;
    tree_t *t6;

    // Based on figure 12.2
    tree_t *tree =
        s(15, t6 = s(6, s(3, s(2, NULL, NULL),
                             s(4, NULL, NULL)),
                        s(7, NULL,
                             t13 = s(13, t9 = s(9, NULL, NULL),
                                         NULL))),
              s(18, t17 = s(17, NULL, NULL),
                    s(20, NULL, NULL)));

    ASSERT_EQUALS(predecessor(t13)->key, 9);
    ASSERT_EQUALS(predecessor(t17)->key, 15);
    ASSERT_EQUALS(predecessor(t9)->key, 7);
    ASSERT_EQUALS(predecessor(t6)->key, 4);
    ASSERT_EQUALS(predecessor(tree)->key, 13);
}

