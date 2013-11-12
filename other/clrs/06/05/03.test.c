#include "03.c"
#include "../../build/ext/test.h"

TEST(heap_minumum) {
    int numbers[] = {1, 2, 4};
    heap_t heap = {numbers, 3, 3};

    ASSERT_EQUALS(heap_minimum(&heap), 1);
}

TEST(heap_extract_min) {
    int actual[]   = {1, 2, 4, 3, 5, 7},
        expected[] = {2, 3, 4, 7, 5};

    heap_t heap = {actual, 6, 6};

    int min = heap_extract_min(&heap);

    ASSERT_EQUALS(min, 1);
    ASSERT_EQUALS(heap.heap_size, 5);
    ASSERT_SAME_ARRAYS_S(heap.elements, expected, 5);
}

TEST(heap_decrease_key) {
    int actual[]   = {1, 2, 4, 3, 5},
        expected[] = {0, 1, 4, 3, 2};

    heap_t heap = {actual, 5, 5};
    heap_decrease_key(&heap, 4, 0);

    ASSERT_SAME_ARRAYS(actual, expected);
}

TEST(min_heap_insert) {
    int actual[]   = {1, 3, 4, 6, 5, -1},
        expected[] = {1, 3, 2, 6, 5, 4};

    heap_t heap = {actual, 6, 5};
    min_heap_insert(&heap, 2);

    ASSERT_SAME_ARRAYS(actual, expected);
}
