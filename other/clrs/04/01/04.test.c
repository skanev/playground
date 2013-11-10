#include "04.c"
#include "../../build/ext/test.h"

TEST(chapter_example) {
    int array[] = { 13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7 };
    max_subarray result = find_maximum_subarray(array, 0, sizeof(array) / sizeof(int));

    ASSERT_EQUALS(result.left, 7);
    ASSERT_EQUALS(result.right, 11);
    ASSERT_EQUALS(result.sum, 43);
}

TEST(negative_numbers) {
    int array[] = { -4, -2, -8, -1, -2, -5 };
    max_subarray result = find_maximum_subarray(array, 0, sizeof(array) / sizeof(int));

    ASSERT_EQUALS(result.left, result.right);
    ASSERT_EQUALS(result.sum, 0);
}

TEST(trivial_case_negative_numbers) {
    int array[] = { -4 };
    max_subarray result = find_maximum_subarray(array, 0, sizeof(array) / sizeof(int));

    ASSERT_EQUALS(result.sum, 0);
    ASSERT_EQUALS(result.left, result.right);
}

TEST(trivial_case_positive_numbers) {
    int array[] = { 4 };
    max_subarray result = find_maximum_subarray(array, 0, sizeof(array) / sizeof(int));

    ASSERT_EQUALS(result.left, 0);
    ASSERT_EQUALS(result.right, 1);
    ASSERT_EQUALS(result.sum, 4);
}
