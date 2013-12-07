#include <stdlib.h>

typedef int jug;

static int tmp;
#define EXCHANGE(a, b) {tmp = a; a = b; b = tmp;}

int cmp(jug red, jug blue);

void quadratic_pair(jug *red, jug *blue, int size) {
    for (int i = 0; i < size; i++) {
        for (int j = i; j < size; j++) {
            if (cmp(red[i], blue[j]) == 0) {
                EXCHANGE(blue[i], blue[j]);
                break;
            }
        }
    }
}

int partition(jug *red, jug *blue, int p, int q);

void quick_pair(jug *red, jug *blue, int p, int r) {
    if (p < r - 1) {
        int q = partition(red, blue, p, r);
        quick_pair(red, blue, p, q);
        quick_pair(red, blue, q + 1, r);
    }
}

int partition(jug *red, jug *blue, int p, int q) {
    int pivot, i;
    jug red_pivot, blue_pivot;

    // Pick a red pivot
    i = rand() % (q - p) + p;
    EXCHANGE(red[i], red[q - 1]);
    red_pivot = red[q - 1];

    // Find the blue pivot and put it in final position
    // NOTE: This look can be folded in the next one to minimize the number of
    // comparisons, but I will keep it here for clarity
    for (int i = p; i < q; i++) {
        if (cmp(red_pivot, blue[i]) == 0) {
            EXCHANGE(blue[i], blue[q - 1]);
            break;
        }
    }

    // Partition the blue jugs around the red pivot
    pivot = p;
    for (int i = p; i < q - 1; i++) {
        if (cmp(red_pivot, blue[i]) > 0) {
            EXCHANGE(blue[pivot], blue[i]);
            pivot++;
        }
    }

    // Put the blue pivot in place
    EXCHANGE(blue[pivot], blue[q-1]);
    blue_pivot = blue[pivot];

    // Partition the red jugs around the blue pivot
    int j = p;
    for (int i = p; i < q - 1; i++) {
        if (cmp(red[i], blue_pivot) < 0) {
            EXCHANGE(red[j], red[i]);
            j++;
        }
    }

    // Put the red pivot in place
    EXCHANGE(red[q - 1], red[j]);

    // Return the pivot index
    return pivot;
}

int cmp(jug red, jug blue) {
    return red - blue;
}
