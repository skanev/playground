#include "06.c"
#include "../../build/ext/test.h"

TEST(heap_increase_key) {
    int actual[]   = {16, 14, 10,  8, 7, 9, 3, 2, 4, 1},
        expected[] = {16, 15, 10, 14, 7, 9, 3, 2, 8, 1};

    heap_t heap = {actual, 10, 10};
    heap_increase_key(&heap, 8, 15);

    ASSERT_SAME_ARRAYS(actual, expected);
}
