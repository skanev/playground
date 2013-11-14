#include "03.c"
#include "../../build/ext/test.h"

TEST(extract_min) {
    int actual[]   = {  1,   3,   7,   8,
                        2,   5,   9, 100,
                        4, 100, 100, 100},
        expected[] = {  2,   3,   7,   8,
                        4,   5,   9, 100,
                      100, 100, 100, INT_MAX};

    tableau_t tableau = {actual, 3, 4};

    int min = extract_min(&tableau);

    ASSERT_EQUALS(min, 1);
    ASSERT_SAME_ARRAYS_S(tableau.elements, expected, 12);
}

TEST(insert) {
    int actual[]   = {  1,   3,   7,   8,
                        2,   5,  10, 100,
                        4, 100, 100, INT_MAX},
        expected[] = {  1,   3,   7,   8,
                        2,   5,   9,  10,
                        4, 100, 100, 100};

    tableau_t tableau = {actual, 3, 4};

    insert(&tableau, 9);

    ASSERT_SAME_ARRAYS_S(tableau.elements, expected, 12);
}

TEST(sort) {
    int expected[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16},
        actual[]   = {9, 6, 13, 8, 16, 2, 14, 5, 4, 11, 3, 1, 12, 7, 10, 15};

    sort(actual, 4);

    ASSERT_SAME_ARRAYS(expected, actual);
}

TEST(find) {
    int numbers[] = {  1,   3,   7,   8,
                       2,   5,  10, 100,
                       4, 100, 100, INT_MAX};

    tableau_t tableau = {numbers, 3, 4};

    ASSERT_TRUE(find(&tableau, 1));
    ASSERT_TRUE(find(&tableau, 2));
    ASSERT_TRUE(find(&tableau, 3));
    ASSERT_TRUE(find(&tableau, 4));
    ASSERT_TRUE(find(&tableau, 5));
    ASSERT_TRUE(find(&tableau, 7));
    ASSERT_TRUE(find(&tableau, 8));
    ASSERT_TRUE(find(&tableau, 10));

    ASSERT_FALSE(find(&tableau, 0));
    ASSERT_FALSE(find(&tableau, 6));
    ASSERT_FALSE(find(&tableau, 9));
    ASSERT_FALSE(find(&tableau, 11));
}
