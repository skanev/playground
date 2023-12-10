#define PARENT(i) ((i - 1) / 2)
#define LEFT(i)   (2 * i + 1)
#define RIGHT(i)  (2 * i + 2)

typedef struct {
    int *nodes;
    int length;
    int heap_size;
} heap;

void max_heapify(heap A, int i) {
    int left, right, largest, temp;

    while(1) {
        left  = LEFT(i);
        right = RIGHT(i);

        if (left < A.heap_size && A.nodes[left] > A.nodes[i])
            largest = left;
        else
            largest = i;

        if (right < A.heap_size && A.nodes[right] > A.nodes[largest])
            largest = right;

        if (largest == i)
            return;

        temp = A.nodes[i];
        A.nodes[i] = A.nodes[largest];
        A.nodes[largest] = temp;

        i = largest;
    }
}
