#include "07.c"
#include "../../build/ext/test.h"

#include <limits.h>
#include <fcntl.h>
#include <unistd.h>

#define S 8
#define R (2 * S * S)
#define SIZE (R * S)
#define SEED 301

void assert_sorted(number *A, size_t size);
void randomize(number *A, size_t size);

TEST(single_process_columnsort) {
    number numbers[SIZE];
    randomize(numbers, SIZE);
    columnsort(numbers, R, S, threaded_sort_columns);
    assert_sorted(numbers, SIZE);
}

TEST(paralel_sort) {
    number numbers[SIZE];
    randomize(numbers, SIZE);
    columnsort(numbers, R, S, threaded_sort_columns);
    assert_sorted(numbers, SIZE);
}

void assert_sorted(number *A, size_t size) {
    for (size_t i = 0; i < size - 1; i++) {
        if (A[i] > A[i + 1]) {
            FAIL("Not sorted at index %lu", i);
        }
    }
}

void randomize(number *A, size_t size) {
    srand(SEED);
    for (size_t i = 0; i < size; i++) {
        A[i] = 0;
        for (size_t j = 0; j < sizeof(number); j += sizeof(int)) {
            A[i] <<= sizeof(number);
            A[i] += rand();
        }
    }
}

void print_mesh(number *A, size_t r, size_t s) {
    size_t size = r * s;
    number max = 0;

    for (size_t i = 0; i < size; i++) {
        if (A[i] > max) {
            max = A[i];
        }
    }

    char format[10];
    sprintf(format, "%%%dd", (int) (log10(max) + 3));

    for (size_t i = 0; i < r; i++) {
        for (size_t j = 0; j < s; j++) {
            printf(format, A[i + j * r]);
        }
        puts("");
    }
}
