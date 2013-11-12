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

void heap_increase_key(heap_t *heap, int i, int key) {
    if (key < heap->elements[i]) {
        fprintf(stderr, "new key is larger than current key");
        exit(0);
    }

    while (i > 0 && heap->elements[PARENT(i)] < key) {
        heap->elements[i] = heap->elements[PARENT(i)];
        i = PARENT(i);
    }
    heap->elements[i] = key;
}
