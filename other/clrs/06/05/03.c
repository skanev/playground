#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define PARENT(i) ((i - 1) / 2)
#define LEFT(i)   (2 * i + 1)
#define RIGHT(i)  (2 * i + 2)

typedef struct {
    int *elements;
    int length;
    int heap_size;
} heap_t;

int heap_minimum(heap_t *heap) {
    return heap->elements[0];
}

void min_heapify(heap_t *heap, int i) {
    int left  = LEFT(i),
        right = RIGHT(i),
        smallest;

    if (left < heap->heap_size && heap->elements[left] < heap->elements[i]) {
        smallest = left;
    } else {
        smallest = i;
    }

    if (right < heap->heap_size && heap->elements[right] < heap->elements[smallest]) {
        smallest = right;
    }

    if (smallest != i) {
        int tmp = heap->elements[i];
        heap->elements[i] = heap->elements[smallest];
        heap->elements[smallest] = tmp;

        min_heapify(heap, smallest);
    }
}

int heap_extract_min(heap_t *heap) {
    if (heap->heap_size == 0) {
        fprintf(stderr, "heap underflow");
        exit(0);
    }

    int min = heap->elements[0];
    heap->elements[0] = heap->elements[heap->heap_size - 1];
    heap->heap_size--;
    min_heapify(heap, 0);

    return min;
}

void heap_decrease_key(heap_t *heap, int i, int key) {
    if (key > heap->elements[i]) {
        fprintf(stderr, "new key is larger than current key");
        exit(0);
    }

    heap->elements[i] = key;
    while (i > 0 && heap->elements[PARENT(i)] > heap->elements[i]) {
        int tmp = heap->elements[PARENT(i)];
        heap->elements[PARENT(i)] = heap->elements[i];
        heap->elements[i] = tmp;
        i = PARENT(i);
    }
}

void min_heap_insert(heap_t *heap, int key) {
    if (heap->length == heap->heap_size) {
        fprintf(stderr, "heap overflow");
        exit(0);
    }

    heap->elements[heap->heap_size] = INT_MAX;
    heap->heap_size++;
    heap_decrease_key(heap, heap->heap_size - 1, key);
}
