#include <limits.h>

#define CROSSOVER_POINT 37

// A struct to represent the tuple

typedef struct {
    unsigned left;
    unsigned right;
    int sum;
} max_subarray;

// The brute force approach

max_subarray find_maximum_subarray_brute(int A[], unsigned low, unsigned high) {
    max_subarray result = {0, 0, INT_MIN};

    for (int i = low; i < high; i++) {
        int current_sum = 0;
        for (int j = i; j < high; j++) {
            current_sum += A[j];
            if (result.sum < current_sum) {
                result.left = i;
                result.right = j + 1;
                result.sum = current_sum;
            }
        }
    }

    return result;
}

// The divide-and-conquer solution

max_subarray find_max_crossing_subarray(int A[], unsigned low, unsigned mid, unsigned high) {
    max_subarray result = {-1, -1, 0};
    int sum = 0,
        left_sum = INT_MIN,
        right_sum = INT_MIN;

    for (int i = mid - 1; i >= (int) low; i--) {
        sum += A[i];
        if (sum > left_sum) {
            left_sum = sum;
            result.left = i;
        }
    }

    sum = 0;

    for (int j = mid; j < high; j++) {
        sum += A[j];
        if (sum > right_sum) {
            right_sum = sum;
            result.right = j + 1;
        }
    }

    result.sum = left_sum + right_sum;
    return result;
}

max_subarray find_maximum_subarray(int A[], unsigned low, unsigned high) {
    if (high == low + 1) {
        max_subarray result = {low, high, A[low]};
        return result;
    } else {
        unsigned mid = (low + high) / 2;
        max_subarray left = find_maximum_subarray(A, low, mid);
        max_subarray right = find_maximum_subarray(A, mid, high);
        max_subarray cross = find_max_crossing_subarray(A, low, mid, high);

        if (left.sum >= right.sum && left.sum >= cross.sum) {
            return left;
        } else if (right.sum >= left.sum && right.sum >= cross.sum) {
            return right;
        } else {
            return cross;
        }
    }
}

// The mixed algorithm

max_subarray find_maximum_subarray_mixed(int A[], unsigned low, unsigned high) {
    if (high - low < CROSSOVER_POINT) {
        return find_maximum_subarray_brute(A, low, high);
    } else {
        unsigned mid = (low + high) / 2;
        max_subarray left = find_maximum_subarray_mixed(A, low, mid);
        max_subarray right = find_maximum_subarray_mixed(A, mid, high);
        max_subarray cross = find_max_crossing_subarray(A, low, mid, high);

        if (left.sum >= right.sum && left.sum >= cross.sum) {
            return left;
        } else if (right.sum >= left.sum && right.sum >= cross.sum) {
            return right;
        } else {
            return cross;
        }
    }
}
