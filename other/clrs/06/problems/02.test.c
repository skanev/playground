#include "02.c"
#include "../../build/ext/test.h"

TEST(extract_min) {
    int actual[]   = {13,   12, 10, 11,   5, 9, 8,   1, 7, 2,   4, 6, 3},
        expected[] = {12,    9, 10, 11,   5, 3, 8,   1, 7, 2,   4, 6};

    heap_t heap = {actual, 3, 13, 13};

    int max = extract_max(&heap);
    ASSERT_EQUALS(max, 13);
    ASSERT_EQUALS(heap.heap_size, 12);
    ASSERT_SAME_ARRAYS_S(heap.elements, expected, 12);
}

TEST(insert) {
    int actual[]   = {14,   12, 10, 11,   5, 9, 8,   1, 7, 2,   4, 6, -1},
        expected[] = {14,   12, 10, 13,   5, 9, 8,   1, 7, 2,   4, 6, 11};

    heap_t heap = {actual, 3, 12, 13};

    insert(&heap, 13);
    ASSERT_EQUALS(heap.heap_size, 13);
    ASSERT_SAME_ARRAYS_S(heap.elements, expected, 13);
}
