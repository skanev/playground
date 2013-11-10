#include <limits.h>

typedef struct {
    unsigned left;
    unsigned right;
    int sum;
} max_subarray;

max_subarray find_max_crossing_subarray(int A[], unsigned low, unsigned mid, unsigned high) {
    max_subarray result = {mid + 1, mid, 0};
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

    if (left_sum + right_sum < 0) {
        max_subarray empty = { mid, mid, 0 };
        return empty;
    } else {
        result.sum = left_sum + right_sum;
        return result;
    }
}

max_subarray find_maximum_subarray(int A[], unsigned low, unsigned high) {
    if (high == low + 1) {
        if (A[low] < 0) {
            max_subarray empty = {low, low, 0};
            return empty;
        } else {
            max_subarray result = {low, high, A[low]};
            return result;
        }
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
