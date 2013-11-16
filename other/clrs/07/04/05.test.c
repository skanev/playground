#include "05.c"
#include "../../build/ext/test.h"

TEST(trivial_case) {
    int array[] = {13, 19, 3, 5, 12, 8, 7, 4, 21, 2, 6, 11},
        expected[] = {2, 3, 4, 5, 6, 7, 8, 11, 12, 13, 19, 21};

    modified_quicksort(array, 0, sizeof(array) / sizeof(int));

    ASSERT_SAME_ARRAYS(array, expected);
}
