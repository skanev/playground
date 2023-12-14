#include "05.c"
#include "../../build/ext/test.h"

#include <stdlib.h>

tree_t* make_tree(int key) {
    tree_t *new = malloc(sizeof(tree_t));
    new->key    = key;
    new->left   = NULL;
    new->right  = NULL;
    new->parent = NULL;
    return new;
}

tree_t* make_left(tree_t *parent, int key) {
    tree_t *new = make_tree(key);
    new->parent = parent;
    parent->left = new;
    return new;
}

tree_t* make_right(tree_t *parent, int key) {
    tree_t *new = make_tree(key);
    new->parent = parent;
    parent->right = new;
    return new;
}

TEST(quote_printing_unquote) {
    tree_t *t1 = make_tree(1);
    tree_t *t2 = make_left(t1, 2);
    tree_t *t3 = make_right(t1, 3);
    tree_t *t4 = make_left(t2, 4);
    tree_t *t5 = make_left(t3, 5);
    tree_t *t6 = make_right(t3, 6);
    int expected[] = {1, 2, 4, 3, 5, 6};

    print_tree(t1);

    ASSERT_SAME_ARRAYS_S(keys, expected, 6);
}
