#include "04.c"
#include "../../build/ext/test.h"

#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

void randomize_array(int array[], unsigned length, unsigned int seed);
bool is_sorted(int array[], int length);

TEST(sorting) {
    int array[]    = {10, 19,  3,  5, 10,  8, 17,  4, 10,  2, 16, 10},
        expected[] = { 2,  3,  4,  5,  8, 10, 10, 10, 10, 16, 17, 19};

    tail_recursive_quicksort(array, 0, sizeof(array) / sizeof(int));

    ASSERT_SAME_ARRAYS(array, expected);
}

TEST(stack_depth) {
    int array[]    = {2, 1, 3, 4, 5, 6, 7, 8, 9, 10},
        expected[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    reset_stack_depth_counter();

    tail_recursive_quicksort(array, 0, sizeof(array) / sizeof(int));

    ASSERT_SAME_ARRAYS(array, expected);
    ASSERT_TRUE(max_stack_depth < 4);
}

TEST(large_array) {
    int size = 100000,
        seed = 300,
        array[size];

    reset_stack_depth_counter();
    randomize_array(array, size, seed);

    tail_recursive_quicksort(array, 0, size);

    ASSERT_TRUE(is_sorted(array, size));
    ASSERT_TRUE(max_stack_depth <= log2(size));
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
