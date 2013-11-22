#include "06.c"
#include "../../build/ext/test.h"

TEST(intersects) {
    ASSERT_TRUE(intersects((interval) {1, 3}, (interval) {0, 2}));
    ASSERT_TRUE(intersects((interval) {2, 4}, (interval) {1, 3}));
    ASSERT_TRUE(intersects((interval) {0, 4}, (interval) {1, 3}));
    ASSERT_TRUE(intersects((interval) {1, 3}, (interval) {0, 4}));

    ASSERT_FALSE(intersects((interval) {1, 2}, (interval) {3, 4}));
    ASSERT_FALSE(intersects((interval) {2, 3}, (interval) {0, 1}));
}

void randomize_array(interval[], unsigned, unsigned int);
bool is_sorted(interval[], int);

TEST(sorting) {
    int seed = 300,
        size = 100000;
    interval array[size];

    randomize_array(array, size, seed);
    fuzzy_sort(array, 0, size);

    ASSERT_TRUE(is_sorted(array, size));
}

void randomize_array(interval array[], unsigned length, unsigned int seed) {
    srand(seed);
    for (unsigned i = 0; i < length; i++) {
        array[i].left  = rand() % 10000 + 1;
        array[i].right = array[i].left + rand() % 5000;
    }
}

bool is_sorted(interval array[], int length) {
    for (int i = 1; i < length; i++) {
        if (!(before(array[i - 1], array[i]) || intersects(array[i - 1], array[i]))) {
            return false;
        }
    }
    return true;
}
