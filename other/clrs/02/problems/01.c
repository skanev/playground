#include <stdlib.h>
#include <string.h>

#define INSERTION_SORT_TRESHOLD 20
#define SELECTION_SORT_TRESHOLD 15

void merge(int A[], int p, int q, int r) {
    int i, j, k;

    int n1 = q - p + 1;
    int n2 = r - q;

#ifdef MERGE_HEAP_ALLOCATION
    int *L = calloc(n1, sizeof(int));
    int *R = calloc(n2, sizeof(int));
#else
    int L[n1];
    int R[n2];
#endif

    memcpy(L, A + p, n1 * sizeof(int));
    memcpy(R, A + q + 1, n2 * sizeof(int));

    for(i = 0, j = 0, k = p; k <= r; k++) {
        if (i == n1) {
            A[k] = R[j++];
        } else if (j == n2) {
            A[k] = L[i++];
        } else if (L[i] <= R[j]) {
            A[k] = L[i++];
        } else {
            A[k] = R[j++];
        }
    }

#ifdef MERGE_HEAP_ALLOCATION
    free(L);
    free(R);
#endif
}

void merge_sort(int A[], int p, int r) {
    if (p < r) {
        int q = (p + r) / 2;
        merge_sort(A, p, q);
        merge_sort(A, q + 1, r);
        merge(A, p, q, r);
    }
}

void insertion_sort(int A[], int p, int r) {
    int i, j, key;

    for (j = p + 1; j <= r; j++) {
        key = A[j];
        i = j - 1;
        while (i >= p && A[i] > key) {
            A[i + 1] = A[i];
            i = i - 1;
        }
        A[i + 1] = key;
    }
}

void selection_sort(int A[], int p, int r) {
    int min, temp;
    for (int i = p; i < r; i++) {
        min = i;
        for (int j = i + 1; j <= r; j++)
            if (A[j] < A[min])
                min = j;
        temp = A[i];
        A[i] = A[min];
        A[min] = temp;
    }
}

void mixed_sort_insertion(int A[], int p, int r) {
    if (p >= r) return;

    if (r - p < INSERTION_SORT_TRESHOLD) {
        insertion_sort(A, p, r);
    } else {
        int q = (p + r) / 2;
        mixed_sort_insertion(A, p, q);
        mixed_sort_insertion(A, q + 1, r);
        merge(A, p, q, r);
    }
}

void mixed_sort_selection(int A[], int p, int r) {
    if (p >= r) return;

    if (r - p < SELECTION_SORT_TRESHOLD) {
        selection_sort(A, p, r);
    } else {
        int q = (p + r) / 2;
        mixed_sort_selection(A, p, q);
        mixed_sort_selection(A, q + 1, r);
        merge(A, p, q, r);
    }
}
