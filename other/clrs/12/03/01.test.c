#include "01.c"
#include "../../build/ext/test.h"

#include <stdlib.h>

TEST(insert) {
    tree_t *tree = make_tree();
    insert(tree, 7);
    insert(tree, 3);
    insert(tree, 5);
    insert(tree, 1);
    insert(tree, 9);
    insert(tree, 11);

    ASSERT_NOT_NULL(search(tree, 3));
    ASSERT_NOT_NULL(search(tree, 5));
    ASSERT_NOT_NULL(search(tree, 7));
    ASSERT_NOT_NULL(search(tree, 9));
    ASSERT_NOT_NULL(search(tree, 11));

    ASSERT_NULL(search(tree, 2));
    ASSERT_NULL(search(tree, 4));
    ASSERT_NULL(search(tree, 6));
}
