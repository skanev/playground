#include "04.c"
#include "../../build/ext/test.h"

TEST(trivial_case) {
    int array[] = {1},
        inversions = merge_sort(array, 0, 1);

    ASSERT_EQUALS(inversions, 0);
}

TEST(problem_example) {
    int array[]    = {2, 3, 8, 6, 1},
        inversions = merge_sort(array, 0, 4);

    ASSERT_EQUALS(inversions, 5);
}

TEST(chapter_example) {
    int array[]    = {5, 2, 4, 7, 1, 3, 2, 6},
        inversions = merge_sort(array, 0, 7);

    ASSERT_EQUALS(inversions, 14);
}


TEST(reversed_array) {
    int array[]    = {9, 8, 7, 6, 5, 4, 3, 2, 1, 0},
        inversions = merge_sort(array, 0, 9);

    ASSERT_EQUALS(inversions, 45);
}
