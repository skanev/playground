#include "05.c"
#include "../../build/ext/test.h"

TEST(array_empty) {
    int array[] = {};
    int length = 0;

    ASSERT_EQUALS(binary_search(array, length, 1), -1);
}

TEST(array_one_element) {
    int array[] = {1};
    int length = 1;

    ASSERT_EQUALS(binary_search(array, length, 1), 0);

    ASSERT_EQUALS(binary_search(array, length, 0), -1);
    ASSERT_EQUALS(binary_search(array, length, 2), -1);
}

TEST(array_odd_elements) {
    int array[] = {2, 4, 6, 8, 10};
    int length = 5;

    ASSERT_EQUALS(binary_search(array, length, 2), 0);
    ASSERT_EQUALS(binary_search(array, length, 4), 1);
    ASSERT_EQUALS(binary_search(array, length, 6), 2);
    ASSERT_EQUALS(binary_search(array, length, 8), 3);
    ASSERT_EQUALS(binary_search(array, length, 10), 4);

    ASSERT_EQUALS(binary_search(array, length, 1), -1);
    ASSERT_EQUALS(binary_search(array, length, 3), -1);
    ASSERT_EQUALS(binary_search(array, length, 5), -1);
    ASSERT_EQUALS(binary_search(array, length, 7), -1);
    ASSERT_EQUALS(binary_search(array, length, 9), -1);
    ASSERT_EQUALS(binary_search(array, length, 11), -1);
}

TEST(array_even_elements) {
    int array[] = {2, 4, 6, 8, 10, 12};
    int length = 6;

    ASSERT_EQUALS(binary_search(array, length, 2), 0);
    ASSERT_EQUALS(binary_search(array, length, 4), 1);
    ASSERT_EQUALS(binary_search(array, length, 6), 2);
    ASSERT_EQUALS(binary_search(array, length, 8), 3);
    ASSERT_EQUALS(binary_search(array, length, 10), 4);
    ASSERT_EQUALS(binary_search(array, length, 12), 5);

    ASSERT_EQUALS(binary_search(array, length, 1), -1);
    ASSERT_EQUALS(binary_search(array, length, 3), -1);
    ASSERT_EQUALS(binary_search(array, length, 5), -1);
    ASSERT_EQUALS(binary_search(array, length, 7), -1);
    ASSERT_EQUALS(binary_search(array, length, 9), -1);
    ASSERT_EQUALS(binary_search(array, length, 11), -1);
    ASSERT_EQUALS(binary_search(array, length, 13), -1);
}
