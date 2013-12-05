#include "02.c"
#include "../../build/ext/test.h"

#include <stdlib.h>

#define SEED 300
#define SIZE 10000
#define RANGE 200

void generate_binary_array(item *A, int size, int from, int to);
void assert_sorted(item *A, int size);
void assert_stable(item *A, int size);

TEST(stable_linear) {
    item items[SIZE];
    generate_binary_array(items, SIZE, 0, 1);

    stable_linear_sort(items, SIZE);

    assert_sorted(items, SIZE);
    assert_stable(items, SIZE);
}

TEST(linear_in_place) {
    item items[SIZE];
    generate_binary_array(items, SIZE, 0, 1);

    linear_in_place_sort(items, SIZE);

    assert_sorted(items, SIZE);
}

TEST(stable_in_place) {
    item items[SIZE];
    generate_binary_array(items, SIZE, 0, 1);

    stable_in_place_sort(items, SIZE);

    assert_sorted(items, SIZE);
    assert_stable(items, SIZE);
}

TEST(in_place_counting_sort) {
    item items[SIZE];
    generate_binary_array(items, SIZE, 1, RANGE);

    in_place_counting_sort(items, SIZE, RANGE);

    assert_sorted(items, SIZE);
}

int next_id();

void generate_binary_array(item *A, int size, int from, int to) {
    srand(SEED);
    for (int i = 0; i < size; i++) {
        A[i].key = rand() % (to - from + 1) + from;
        A[i].value = next_id();
    }
}

void assert_sorted(item *A, int size) {
    for (int i = 0; i < size - 1; i++) {
        if (A[i].key > A[i+1].key) {
            FAIL("Not sorted at index %d: %d > %d", i, A[i].key, A[i+1].key);
        }
    }
}

void assert_stable(item *A, int size) {
    for (int i = 0; i < size - 1; i++) {
        if (A[i].key == A[i + 1].key && A[i].value > A[i+1].value) {
            FAIL("Not stable at index %d: %d > %d", i, A[i].value, A[i+1].value);
        }
    }
}

int next_id() {
    static int id = 1;
    return id++;
}
