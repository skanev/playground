#include <stdlib.h>

static int tmp;
#define EXCHANGE(a, b) { tmp = a; a = b; b = tmp; }

int randomized_partition(int *A, int p, int r);

int randomized_select(int *A, int p, int r, int i) {
    while (p < r - 1) {
        int q = randomized_partition(A, p, r);
        int k = q - p;

        if (i == k) {
            return A[q];
        } else if (i < k) {
            r = q;
        } else {
            p = q + 1;
            i = i - k - 1;
        }
    }

    return A[p];
}

int partition(int *A, int p, int r) {
    int x, i, j;

    x = A[r - 1];
    i = p;

    for (j = p; j < r - 1; j++) {
        if (A[j] < x) {
            EXCHANGE(A[i], A[j]);
            i++;
        }
    }

    EXCHANGE(A[i], A[r - 1]);

    return i;
}

int randomized_partition(int *A, int p, int r) {
    int pivot = rand() % (r - p) + p;
    EXCHANGE(A[pivot], A[r - 1]);
    return partition(A, p, r);
}
