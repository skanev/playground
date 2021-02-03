#include "02.c"
#include "../../build/ext/test.h"

#include <string.h>
#include <stdlib.h>

#define N 10
const char *actual[N];
int count = 0;

void append(const char *s) {
    actual[count++] = s;
}

TEST(sort) {
    char *s1011 = "1011";
    char *s10 = "10";
    char *s011 = "011";
    char *s100 = "100";
    char *s0 = "0";

    radix_tree_t *tree = make_radix_tree();

    insert(tree, s1011);
    insert(tree, s10);
    insert(tree, s011);
    insert(tree, s100);
    insert(tree, s0);

    walk_sorted(tree, append);

    ASSERT_TRUE(strcmp(s0, actual[0]) == 0);
    ASSERT_TRUE(strcmp(s011, actual[1]) == 0);
    ASSERT_TRUE(strcmp(s10, actual[2]) == 0);
    ASSERT_TRUE(strcmp(s100, actual[3]) == 0);
    ASSERT_TRUE(strcmp(s1011, actual[4]) == 0);
}
