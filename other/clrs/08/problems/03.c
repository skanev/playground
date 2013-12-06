#include <math.h>
#include <string.h>

#define MAX_LENGTH 10

// --- Structs and typedefs ---------------------------------------------------

// In order to simplify everything, both numbers and strings are meshed in a
// single union called key_t. The key does not know whether it is a number or a
// string - the handling code already knows it instead.

union key_t {
    int number;
    char string[MAX_LENGTH + 1];
};

typedef struct {
    union key_t key;
    int value;
} item;

typedef int (*key_f)(item, int);
typedef int (*dimension_f)(item);
typedef int (*compare_f)(item, item);

// --- Prototypes -------------------------------------------------------------

// Various sorting functinos

void partition(item *A, int size, int digits, int *groups, dimension_f dimension);
void radix_sort(item *A, int left, int right, int digits, key_f key);
void counting_sort(item *A, int left, int right, int dimension, key_f key, int key_index);

// Functions to work on numbers

int item_nth_digit(item i, int d);
int item_digits(item i);

// Functions to work on strings

int item_string_length(item i);
int item_nth_char(item i, int d);

// --- The solutions ----------------------------------------------------------

void sort_numbers(item *A, int size, int max_digits) {
    int groups[max_digits + 1];

    partition(A, size, max_digits, groups, item_digits);

    for (int i = 1; i < max_digits + 1; i++) {
        radix_sort(A, groups[i - 1], groups[i], i, item_nth_digit);
    }
}

void sort_strings(item *A, int size, int max_length) {
    int groups[max_length + 1];

    partition(A, size, max_length, groups, item_string_length);

    for (int len = max_length; len > 0; len--) {
        counting_sort(A, groups[len - 1], size, 26, item_nth_char, len - 1);
    }
}

// --- Auxiliary sorting functions --------------------------------------------

// Performs counting sort on a dimension (number of digits or string length)
// and populates a table (groups) with the position of each dimension.

void partition(item *A, int size, int max_dimension, int *groups, dimension_f dimension) {
    int counts[max_dimension + 1];
    item temp[size];

    for (int i = 0; i < max_dimension + 1; i++) { groups[i] = 0; }
    for (int i = 0; i < size;              i++) { groups[dimension(A[i])]++; }
    for (int i = 1; i < max_dimension + 1; i++) { groups[i] += groups[i - 1]; }
    for (int i = 0; i < max_dimension + 1; i++) { counts[i] = groups[i]; }
    for (int i = 0; i < size;              i++) { temp[i] = A[i]; }

    for (int i = size - 1; i >= 0; i--) {
        int d = dimension(temp[i]);
        int count = counts[d];

        A[count - 1] = temp[i];
        counts[d]--;
    }
}

// A simple radix sort

void radix_sort(item *A, int left, int right, int digits, key_f key) {
    for (int i = 0; i < digits; i++) {
        counting_sort(A, left, right, 10, key, i);
    }
}

// A slightly generalized counting sort

void counting_sort(item *A, int left, int right, int dimension, key_f key, int key_index) {
    int size = right - left;
    int counts[dimension];
    item temp[size];

    for (int i = 0;    i < dimension; i++) { counts[i] = 0; }
    for (int i = left; i < right;     i++) { counts[key(A[i], key_index)]++; }
    for (int i = 1;    i < dimension; i++) { counts[i] += counts[i - 1]; }
    for (int i = 0;    i < size;      i++) { temp[i] = A[left + i]; }

    for (int i = size - 1; i >= 0; i--) {
        int n = key(temp[i], key_index);
        int count = counts[n];

        A[left + count - 1] = temp[i];
        counts[n]--;
    }
}

// --- Key handling -----------------------------------------------------------

int count_digits(int n) {
    if (n == 0) {
        return 1;
    } else {
        return (int) log10(n) + 1;
    }
}

int nth_digit(int n, int d) {
    int magnitude = (int) pow(10, d);

    return (n / magnitude) % 10;
}

int item_nth_digit(item i, int d) {
    return nth_digit(i.key.number, d);
}

int item_digits(item i) {
    return count_digits(i.key.number);
}

int item_string_length(item a) {
    return strlen(a.key.string);
}

int item_nth_char(item a, int n) {
    return a.key.string[n] - 'a';
}
