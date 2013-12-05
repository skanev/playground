#include <stdbool.h>

typedef struct {
    int key;
    int value;
} item;

static item tmp;

#define EXCHANGE(a, b) tmp = a; a = b; b = tmp;

void stable_linear_sort(item *A, int size) {
    int zero = 0,
        one  = 0;
    item copy[size];

    for (int i = 0; i < size; i++) {
        if (A[i].key == 0) {
            one++;
        }
    }

    for (int i = 0; i < size; i++) {
        if (A[i].key == 0) {
            copy[zero] = A[i];
            zero++;
        } else {
            copy[one] = A[i];
            one++;
        }
    }

    for (int i = 0; i < size; i++) {
        A[i] = copy[i];
    }
}

void linear_in_place_sort(item *A, int size) {
    int left = -1,
        right = size;

    while (true) {
        do { left++;  } while (A[left].key  == 0);
        do { right--; } while (A[right].key == 1);

        if (left > right) {
            return;
        }

        EXCHANGE(A[left], A[right]);
    }
}

void stable_in_place_sort(item *A, int size) {
    for (int i = size; i > 0; i--) {
        for (int j = 0; j < i; j++) {
            if (A[j].key > A[j + 1].key) {
                EXCHANGE(A[j], A[j+1]);
            }
        }
    }
}

void in_place_counting_sort(item *A, int size, int range) {
    int counts[range + 1];
    int positions[range + 1];

    for (int i = 0; i <= range; i++) {
        counts[i] = 0;
    }

    for (int i = 0; i < size; i++) {
        counts[A[i].key]++;
    }

    for (int i = 2; i <= range; i++) {
        counts[i] += counts[i-1];
    }

    for (int i = 0; i <= range; i++) {
        positions[i] = counts[i];
    }

    int i = 0;
    while (i < size) {
        int key = A[i].key;
        bool placed = (positions[key - 1] <= i && i < positions[key]);

        if (placed) {
            i++;
        } else {
            EXCHANGE(A[i], A[counts[key] - 1]);
            counts[key]--;
        }
    }
}
