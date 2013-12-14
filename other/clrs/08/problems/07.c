#include <pthread.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#define STDLIB_SORT qsort

typedef unsigned int number;

typedef struct {
    size_t start;
    size_t size;
} column_t;

typedef void column_sorter(number *, column_t *, int);

void check_dimensions(size_t r, size_t s);

/**
 * The basic column sort implementation. It does a copy of the array for steps
 * 3 and 5. It also does not sort the half-columns in the beginning and the
 * end, since that is not necessary for the correctness of the algorithm.
 */

void columnsort(number *A, size_t r, size_t s, column_sorter sort_columns) {
    size_t size = r * s;
    number *copy;
    column_t columns[s];

    check_dimensions(r, s);

    copy = calloc(size, sizeof(number));

    for (size_t i = 0; i < s; i++) {
        columns[i] = (column_t) {i * r, r};
    }

    sort_columns(A, columns, s);

    for (size_t i = 0; i < size; i++) {
        copy[(i % s) * r + i / s] = A[i];
    }

    sort_columns(copy, columns, s);

    for (size_t i = 0; i < size; i++) {
        A[i] = copy[(i % s) * r + i / s];
    }

    sort_columns(A, columns, s);

    for (size_t i = 0; i < s - 1; i++) {
        columns[i] = (column_t) {i * r + r / 2, r};
    }

    sort_columns(A, columns, s - 1);

    free(copy);
}

/*
 * A function that compares numbers, to be passed to the stdlib sort.
 */

int compare(const void *a, const void *b) {
    number *first  = (number *) a;
    number *second = (number *) b;

    if (*first == *second) {
        return 0;
    } else if (*first > *second) {
        return 1;
    } else {
        return -1;
    }
}

/*
 * Verified the dimensions of the passed array.
 */

void check_dimensions(size_t r, size_t s) {
    if (r % 2) {
        fprintf(stderr, "r must be even\n");
        exit(0);
    }

    if (r % s) {
        fprintf(stderr, "s must divide r\n");
        exit(0);
    }

    if (r < 2 * s * s) {
        fprintf(stderr, "r must be grater than 2s²\n");
        exit(0);
    }
}

/*
 * A utility function to call with the array and a column.
 */

void sort(number *A, column_t column) {
    STDLIB_SORT(A + column.start, column.size, sizeof(number), compare);
}

/*
 * Sequential sorting of columns
 */

void sequential_sort_columns(number *numbers, column_t *columns, int size) {
    for (int i = 0; i < size; i++) {
        sort(numbers, columns[i]);
    }
}

/*
 * Parallel sorting of columns. This implementation is a bit naïve - it can
 * reuse existing threads instead of spawning new ones every time. Furthermore,
 * I never explored using locking mechanisms instead of joining the threads.
 */

typedef struct {
    number *numbers;
    column_t column;
} job_t;

void *sort_job(void *pjob) {
    job_t *job = (job_t *) pjob;
    sort(job->numbers, job->column);
    return NULL;
}

void threaded_sort_columns(number *numbers, column_t *columns, int size) {
    void *status;
    pthread_t threads[size];
    job_t jobs[size];
    pthread_attr_t attr;

    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

    for (int i = 0; i < size; i++) {
        jobs[i] = (job_t) {numbers, columns[i]};
        pthread_create(&threads[i], &attr, sort_job, &jobs[i]);
    }

    for (int i = 0; i < size; i++) {
        pthread_join(threads[i], &status);
    }
}
