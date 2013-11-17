#include "02.c"
#include "../../build/ext/test.h"

#include <stdlib.h>
#include <stdbool.h>

void randomize_array(int array[], unsigned length, unsigned int seed);
bool is_sorted(int array[], int length);

TEST(partitioning) {
    int array[]    = {10, 19,  3,  5, 10,  8, 17,  4, 10,  2, 16, 10},
        expected[] = { 3,  5,  8,  4,  2, 10, 10, 10, 10, 19, 16, 17};

    pivot_t pivot = partition(array, 0, sizeof(array) / sizeof(int));

    ASSERT_EQUALS(pivot.q, 5);
    ASSERT_EQUALS(pivot.t, 9);
    ASSERT_SAME_ARRAYS(array, expected);
}

TEST(sorting) {
    int array[]    = {10, 19,  3,  5, 10,  8, 17,  4, 10,  2, 16, 10},
        expected[] = { 2,  3,  4,  5,  8, 10, 10, 10, 10, 16, 17, 19};

    quicksort(array, 0, sizeof(array) / sizeof(int));

    ASSERT_SAME_ARRAYS(array, expected);
}

TEST(large_array) {
    int size = 100000,
        seed = 300,
        array[size];

    randomize_array(array, size, seed);

    quicksort(array, 0, size);

    ASSERT_TRUE(is_sorted(array, size));
}

void randomize_array(int array[], unsigned length, unsigned int seed) {
    srand(seed);
    for (unsigned i = 0; i < length; i++) {
        array[i] = rand() % 100 + 1;
    }
}

bool is_sorted(int array[], int length) {
    for (int i = 1; i < length; i++) {
        if (array[i - 1] > array[i]) {
            return false;
        }
    }
    return true;
}
