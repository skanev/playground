#include "06.c"
#include "../../build/ext/test.h"

TEST(large_example) {
    int data[] = {10, 17, 13, 28, 23,
                  17, 22, 16, 29, 23,
                  24, 28, 22, 34, 24,
                  11, 13,  6, 17,  7,
                  45, 44, 32, 37, 23,
                  36, 33, 19, 21,  6,
                  75, 66, 51, 53, 34},
        expected[7] = {0, 2, 2, 2, 4, 4, 4},
        actual[7];
    array A = { 7, 5, 1, data };

    find_minimums(A, actual);
    ASSERT_SAME_ARRAYS(actual, expected);
}

TEST(small_example) {
    int data[] = {37, 23, 24, 32,
                  21,  6,  7, 10,
                  53, 34, 30, 31,
                  32, 13,  9,  6,
                  43, 21, 15,  8},
        expected[5] = {1, 1, 2, 3, 3},
        actual[5];
    array A = { 5, 4, 1, data };

    find_minimums(A, actual);
    ASSERT_SAME_ARRAYS(actual, expected);
}

