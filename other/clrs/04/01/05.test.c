#include <stdlib.h>
#include <limits.h>
#include "05.c"
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

    ASSERT_EQUALS(result.left, 3);
    ASSERT_EQUALS(result.right, 4);
    ASSERT_EQUALS(result.sum, -1);
}

TEST(trivial_case_negative_numbers) {
    int array[] = { -4 };
    max_subarray result = find_maximum_subarray(array, 0, sizeof(array) / sizeof(int));

    ASSERT_EQUALS(result.left, 0);
    ASSERT_EQUALS(result.right, 1);
    ASSERT_EQUALS(result.sum, -4);
}

TEST(trivial_case_positive_numbers) {
    int array[] = { 4 };
    max_subarray result = find_maximum_subarray(array, 0, sizeof(array) / sizeof(int));

    ASSERT_EQUALS(result.left, 0);
    ASSERT_EQUALS(result.right, 1);
    ASSERT_EQUALS(result.sum, 4);
}

void generate_random_array(int array[], unsigned size, unsigned seed);
max_subarray find_maximum_subarray_brute(int A[], unsigned low, unsigned high);

TEST(comparison_with_brute_force) {
    int size = 50;
    int array[size];
    max_subarray expected, actual;

    for (int i = 0; i < 10000; i++) {
        generate_random_array(array, size, i);
        actual   = find_maximum_subarray_brute(array, 0, size);
        expected = find_maximum_subarray(array, 0, size);
        ASSERT_EQUALS(expected.sum, actual.sum);
    }
}

max_subarray find_maximum_subarray_brute(int A[], unsigned low, unsigned high) {
    max_subarray result = {0, 0, INT_MIN};

    for (int i = low; i < high; i++) {
        int current_sum = 0;
        for (int j = i; j < high; j++) {
            current_sum += A[j];
            if (result.sum < current_sum) {
                result.left = i;
                result.right = j + 1;
                result.sum = current_sum;
            }
        }
    }

    return result;
}

void generate_random_array(int array[], unsigned size, unsigned seed) {
    srand(seed);
    for (unsigned i = 0; i < size; i++) {
        array[i] = rand() % 101 - 50;
    }
}
