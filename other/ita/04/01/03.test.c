#include "03.c"
#include "../../build/ext/test.h"

TEST(chapter_example_brute) {
    int array[] = { 13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7 };
    max_subarray result = find_maximum_subarray_brute(array, 0, sizeof(array) / sizeof(int));

    ASSERT_EQUALS(result.left, 7);
    ASSERT_EQUALS(result.right, 11);
    ASSERT_EQUALS(result.sum, 43);
}

TEST(chapter_example_divide_and_conquer) {
    int array[] = { 13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7 };
    max_subarray result = find_maximum_subarray(array, 0, sizeof(array) / sizeof(int));

    ASSERT_EQUALS(result.left, 7);
    ASSERT_EQUALS(result.right, 11);
    ASSERT_EQUALS(result.sum, 43);
}

TEST(chapter_example_mixed) {
    int array[] = { 13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7 };
    max_subarray result = find_maximum_subarray_mixed(array, 0, sizeof(array) / sizeof(int));

    ASSERT_EQUALS(result.left, 7);
    ASSERT_EQUALS(result.right, 11);
    ASSERT_EQUALS(result.sum, 43);
}
