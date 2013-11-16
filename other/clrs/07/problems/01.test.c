#include "01.c"
#include "../../build/ext/test.h"

TEST(partitioning) {
    int array[]    = {13, 19,  9,  5, 12,  8,  7,  4, 11,  2,  6, 21},
        expected[] = { 6,  2,  9,  5, 12,  8,  7,  4, 11, 19, 13, 21};

    int index = hoare_partition(array, 0, sizeof(array) / sizeof(int));

    ASSERT_EQUALS(index, 8);
    ASSERT_SAME_ARRAYS(array, expected);
}

TEST(sorting) {
    int array[]    = {13, 19,  9,  5, 12,  8,  7,  4, 11,  2,  6, 21},
        expected[] = { 2,  4,  5,  6,  7,  8,  9, 11, 12, 13, 19, 21};

    quicksort(array, 0, sizeof(array) / sizeof(int));

    ASSERT_SAME_ARRAYS(array, expected);
}
