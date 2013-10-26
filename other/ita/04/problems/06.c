typedef struct array {
    int m;
    int n;
    int step;
    int *data;
} array;

int get(array A, int i, int j) {
    return A.data[((i + 1) * A.step - 1) * A.n + j];
}

array half(array a) {
    array result = { a.m, a.n, a.step * 2, a.data };
    return result;
}

int height(array array) {
    return array.m / array.step;
}

int min_index(array A, int row, int left, int right) {
    int min = left;

    for (int i = left; i < right; i++) {
        if (get(A, row, i) < get(A, row, min)) {
            min = i;
        }
    }

    return min;
}

void find_minimums(array A, int *mins) {
    if (height(A) == 1) {
        mins[0] = min_index(A, 0, 0, A.n);
    } else {
        array evens = half(A);
        int even_minimums[height(evens)];

        find_minimums(evens, even_minimums);

        int leftmost = 0;

        for (int i = 0; i < height(evens); i++) {
            leftmost = min_index(A, 2 * i, leftmost, even_minimums[i] + 1);

            mins[2 * i]     = leftmost;
            mins[2 * i + 1] = even_minimums[i];
        }

        if (height(A) % 2) {
            mins[height(A) - 1] = min_index(A, height(A) - 1, leftmost, A.n);
        }
    }
}
