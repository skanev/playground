#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define SIZE 400000
#define SEED 300
#define MERGE_HEAP_ALLOCATION

#include "01.c"

#define TIME(message, sort) \
    randomize_array(array, SIZE, SEED); \
    timer_start_time = clock(); \
    sort(array, 0, SIZE - 1); \
    printf(message " = %f\n", (double) (clock() - timer_start_time) / CLOCKS_PER_SEC); \
    check_sorted(array, SIZE);

static clock_t timer_start_time;

void randomize_array(int array[], unsigned length, unsigned int seed) {
    srand(seed);
    for (unsigned i = 0; i < length; i++) {
        array[i] = rand() % 1000 + 1;
    }
}

void check_sorted(int array[], int length) {
    for (int i = 1; i < length; i++) {
        if (array[i - 1] > array[i]) {
            printf("%d %d %d %d\n", i - 1, i, array[i - 1], array[i]);
            fprintf(stderr, "...but the array is not sorted!");
            exit(1);
        }
    }
}

int main() {
    int *array = calloc(SIZE, sizeof(int));

    TIME("merge-sort     ", merge_sort);
    TIME("merge-insertion", mixed_sort_insertion);
    TIME("merge-selection", mixed_sort_selection);

    return 0;
}
