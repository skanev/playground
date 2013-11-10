#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "03.c"
#include "../../build/ext/debug_helpers.h"

#define SIZE CROSSOVER_POINT * 200

#ifndef SEED
#define SEED 300
#endif

static clock_t timer_start_time;

#define TIME(times, message, subarray, size) \
    timer_start_time = clock(); \
    for (int i = 0; i < times; i++) { \
        answer = subarray(array, 0, size); \
    } \
    printf(message " = %f\n", (double) (clock() - timer_start_time) / CLOCKS_PER_SEC); \
    check_right_answer(expected, answer);

void randomize_array(int array[], unsigned length, unsigned int seed) {
    srand(seed);
    for (unsigned i = 0; i < length; i++) {
        array[i] = rand() % 101 - 50;
    }
}

void check_right_answer(max_subarray expected, max_subarray actual) {
    if (expected.sum != actual.sum) {
        printf("%u %u %d\n", expected.left, expected.right, expected.sum);
        printf("%u %u %d\n", actual.left, actual.right, actual.sum);
        fprintf(stderr, "Incorrect result ;(\n");
        exit(1);
    }
}

int main() {
    int array[SIZE];
    randomize_array(array, SIZE, SEED);
    max_subarray expected, answer;

    expected = find_maximum_subarray(array, 0, CROSSOVER_POINT);
    printf("%d elements, 10000 times...\n", CROSSOVER_POINT);
    TIME(10000, "brute-force       ", find_maximum_subarray_brute, CROSSOVER_POINT);
    TIME(10000, "divide-and-conquer", find_maximum_subarray, CROSSOVER_POINT);
    TIME(10000, "mixed             ", find_maximum_subarray_mixed, CROSSOVER_POINT);

    printf("=============================\n");

    expected = find_maximum_subarray(array, 0, SIZE);
    printf("%d elements, 1 time...\n", SIZE);
    TIME(1, "brute-force       ", find_maximum_subarray_brute, SIZE);
    TIME(1, "divide-and-conquer", find_maximum_subarray, SIZE);
    TIME(1, "mixed             ", find_maximum_subarray_mixed, SIZE);

    exit(0);
}
