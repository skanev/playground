#include "05.c"
#include "../../build/ext/test.h"

#include <stdlib.h>

#define N 1000000
#define K 100
#define SEED 300
#define RANGE 1000000

void generate_random_array(int *numbers);
void assert_k_sorted(int *numbers, int k);

TEST(k_sorting) {
    int numbers[N];

    generate_random_array(numbers);
    k_sort(numbers, N, K);
    assert_k_sorted(numbers, K);
}

TEST(merging_k_sorted) {
    int numbers[N];

    generate_random_array(numbers);
    k_sort(numbers, N, K);
    merge_k_sorted(numbers, N, K);
    assert_k_sorted(numbers, 1);
}

void assert_k_sorted(int *numbers, int k) {
    for (int i = 0; i < N - k; i++) {
        if (numbers[i] > numbers[i + k]) {
            FAIL("Array not k-sorted at %d/%d: %d > %d:",
                    i, i + k, numbers[i], numbers[i + k]);
        }
    }
}

void generate_random_array(int *numbers) {
    srand(SEED);

    for (int i = 0; i < N; i++) {
        numbers[i] = rand() % RANGE;
    }
}
