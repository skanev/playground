#include "04.c"
#include "../../build/ext/test.h"

#include <stdlib.h>

tree_t *make_tree(int key) {
    tree_t *new = malloc(sizeof(tree_t));
    new->key = key;
    new->parent  = NULL;
    new->child   = NULL;
    new->sibling = NULL;
    return new;
}

tree_t *make_child(tree_t *parent, int key) {
    tree_t *new = make_tree(key);
    new->parent = parent;
    parent->child = new;
    return new;
}

tree_t *make_sibling(tree_t *left, int key) {
    tree_t *new = make_tree(key);
    new->parent = left->parent;
    left->sibling = new;
    return new;
}

TEST(quote_printing_unquote) {
    tree_t *t1  = make_tree(1);
    tree_t *t2  = make_child(t1, 2);
    tree_t *t3  = make_sibling(t2, 3);
    tree_t *t4  = make_child(t3, 4);
    tree_t *t5  = make_sibling(t4, 5);
    tree_t *t6  = make_sibling(t5, 6);
    tree_t *t7  = make_child(t2, 7);
    tree_t *t8  = make_sibling(t7, 8);
    tree_t *t9  = make_child(t8, 9);
    tree_t *t10 = make_sibling(t9, 10);
    tree_t *t11 = make_sibling(t8, 11);

    int expected[] = {1, 2, 7, 8, 9, 10, 11, 3, 4, 5, 6};

    print_tree(t1);

    ASSERT_SAME_ARRAYS_S(expected, keys, 11);
}
