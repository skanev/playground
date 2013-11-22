#include <stdbool.h>
#include <stdlib.h>

typedef struct {
    int left;
    int right;
} interval;

bool intersects(interval a, interval b) { return a.left <= b.right && b.left <= a.right; }
bool before(interval a, interval b)     { return a.right < b.left; }
bool after(interval a, interval b)      { return a.left > b.right; }

#define EXCHANGE(a, b) tmp = a; a = b; b = tmp;

interval partition(interval A[], int p, int r) {
    int pick, s, t, i;
    interval intersection, tmp;

    // Pick a random interval as a pivot
    pick = p + rand() % (r - p);
    EXCHANGE(A[pick], A[r-1]);
    intersection = A[r-1];

    // Find an intersection of the pivot and other intervals
    for (i = p; i < r - 1; i++) {
        if (intersects(intersection, A[i])) {
            if (A[i].left > intersection.left)
                intersection.left = A[i].left;
            if (A[i].right < intersection.right)
                intersection.right = A[i].right;
        }
    }

    // Classic partition around the intersection
    for (i = s = p; i < r - 1; i++) {
        if (before(A[i], intersection)) {
            EXCHANGE(A[i], A[s]);
            s++;
        }
    }
    EXCHANGE(A[r-1], A[s]);

    // Group intervals including the intersection
    for (t = s + 1, i = r - 1; t <= i;) {
        if (intersects(A[i], intersection)) {
            EXCHANGE(A[t], A[i]);
            t++;
        } else {
            i--;
        }
    }

    return (interval) {s, t};
}

void fuzzy_sort(interval array[], int p, int r) {
    if (p < r - 1) {
        interval pivot = partition(array, p, r);
        fuzzy_sort(array, p, pivot.left);
        fuzzy_sort(array, pivot.right, r);
    }
}

