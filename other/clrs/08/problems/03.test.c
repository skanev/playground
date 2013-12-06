#include "03.c"
#include "../../build/ext/test.h"

#include <stdlib.h>

#define SEED 300
#define N 400000
#define MAX_DIGITS 10

int generate_random_numbers(item *items);
int generate_random_strings(item *items);
void assert_sorted(item *A, int size, compare_f compare);
void assert_stable(item *A, int size, compare_f compare);

int compare_strings(item a, item b);
int compare_numbers(item a, item b);

TEST(sort_numbers) {
    item numbers[N];
    int size = generate_random_numbers(numbers);

    sort_numbers(numbers, size, MAX_DIGITS);

    assert_sorted(numbers, size, compare_numbers);
    assert_stable(numbers, size, compare_numbers);
}

TEST(sort_strings) {
    item strings[N];
    int size = generate_random_strings(strings);

    sort_strings(strings, size, MAX_LENGTH);

    assert_sorted(strings, size, compare_strings);
    assert_stable(strings, size, compare_strings);
}

int compare_strings(item a, item b) {
    return strcmp(a.key.string, b.key.string);
}

int compare_numbers(item a, item b) {
    return a.key.number - b.key.number;
}

void assert_sorted(item *A, int size, compare_f compare) {
    for (int i = 0; i < size - 1; i++) {
        if (compare(A[i], A[i + 1]) > 0) {
            FAIL("Not sorted at index %d", i);
        }
    }
}

void assert_stable(item *A, int size, compare_f compare) {
    for (int i = 0; i < size - 1; i++) {
        if (compare(A[i], A[i + 1]) == 0 && A[i].value > A[i+1].value) {
            FAIL("Not stable at index %d: %d > %d", i, A[i].value, A[i+1].value);
        }
    }
}

int next_id() {
    static int id = 1;
    return id++;
}

int generate_random_numbers(item *numbers) {
    srand(SEED);
    int i = 0,
        digits = 0;

    while (digits < N) {
        int magnitude = rand() % MAX_DIGITS + 1;

        if (magnitude > N - digits) {
            magnitude = N - digits;
        }

        int base   = (int) pow(10, magnitude - 1);
        int number = rand() % ((int) pow(10, magnitude) - base) + base;

        numbers[i].key.number = number;
        numbers[i].value = next_id();

        i++;
        digits += magnitude;
    }

    return i;
}

int generate_random_strings(item *strings) {
    srand(SEED);
    int i = 0,
        chars = 0;

    while (chars < N) {
        int length = rand() % MAX_LENGTH + 1;

        if (length > N - chars) {
            length = N - chars;
        }

        int j;
        for (j = 0; j < length; j++) {
            strings[i].key.string[j] = 'a' + rand() % 26;
        }
        strings[i].key.string[j] = '\0';

        strings[i].value = next_id();

        i++;
        chars += length;
    }

    return i;
}
