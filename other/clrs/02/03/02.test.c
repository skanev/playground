#include "02.c"
#include "../../build/ext/test.h"

TEST(trivial_case) {
    int expected[] = {1},
        actual[]   = {1};

    merge_sort(actual, 0, 1);

    ASSERT_SAME_ARRAYS(actual, expected);
}

TEST(chapter_example) {
    int expected[] = {1, 2, 2, 3, 4, 5, 6, 7},
        actual[]   = {5, 2, 4, 7, 1, 3, 2, 6};

    merge_sort(actual, 0, 7);

    ASSERT_SAME_ARRAYS(actual, expected);
}

TEST(exercise_example) {
    int expected[] = {3, 9, 26, 38, 41, 49, 52, 57},
        actual[]   = {3, 41, 52, 26, 38, 57, 9, 49};

    merge_sort(actual, 0, 7);

    ASSERT_SAME_ARRAYS(actual, expected);
}

TEST(reversed_merge) {
    int actual[]   = {5, 6, 7, 8, 9, 0, 1, 2, 3, 4},
        expected[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

    merge_sort(actual, 0, 9);

    ASSERT_SAME_ARRAYS(actual, expected);
}
