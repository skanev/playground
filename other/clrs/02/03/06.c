// Indices in the C code are different
int binary_search(int A[], int length, int v) {
    int low  = 0;
    int high = length;

    int mid;
    while (low < high) {
        mid = (low + high) / 2;

        if (A[mid] == v)
            return mid;
        else if (A[mid] < v)
            low = mid + 1;
        else
            high = mid;
    }

    return -1;
}
