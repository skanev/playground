#include "05.c"
#include "../../build/ext/test.h"

TEST(chapter_example) {
    int expected[] = {16, 14, 10, 8,  7, 9, 3, 2, 4, 1},
        actual[]   = {16,  4, 10, 14, 7, 9, 3, 2, 8, 1};

    heap A = {actual, 10, 10};

    max_heapify(A, 1);

    ASSERT_SAME_ARRAYS(actual, expected);
}

TEST(exercise_6_2_1_example) {
    int expected[] = {27, 17, 10, 16, 13,  9, 1, 5, 7, 12, 4, 8, 3, 0},
        actual[]   = {27, 17,  3, 16, 13, 10, 1, 5, 7, 12, 4, 8, 9, 0};

    heap A = {actual, 14, 14};

    max_heapify(A, 2);

    ASSERT_SAME_ARRAYS(actual, expected);
}
