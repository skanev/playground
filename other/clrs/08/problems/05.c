#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

typedef struct {
    int value;
    int s;
} item;

typedef struct {
    item *elements;
    int length;
    int heap_size;
} heap_t;

typedef struct {
    int size;
    int k;
    int exhausted;
    int *next_indices;
} sort_state_t;

void merge_sort(int A[], int p, int r, int k, int s);
void min_heap_insert(heap_t *heap, item key);
int state_took_column(sort_state_t *state, int index);
item min_heap_push_pop(heap_t *heap, item new);
item heap_minimum(heap_t *heap);
item heap_extract_min(heap_t *heap);

/*
 * Average soting is performed by just merge-sorting each column. That was
 * easy. Modifying merge sort was hard.
 */

void k_sort(int *numbers, int size, int k) {
    for (int i = 0; i < k; i++) {
        merge_sort(numbers, 0, size, k, i);
    }
}

/*
 * Sorting a k-sorted array. We need to keep track of which column produced
 * the minumum element in the heap and this resulted in quite the tricky C
 * code. I don't think this is a good practice, but still, that's the best I'm
 * willing to make it right now.
 */

void merge_k_sorted(int *numbers, int size, int k) {
    int copy[size];

    item heap_elements[k];
    heap_t heap = {heap_elements, k, 0};

    int next_indices[k];
    sort_state_t state = {size, k, 0, next_indices};

    memcpy(copy, numbers, size * sizeof(int));

    for (int i = 0; i < k; i++) {
        item new = {copy[i], i};
        min_heap_insert(&heap, new);
        next_indices[i] = i + k;
    }

    for (int i = 0; i < size; i++) {
        item min = heap_minimum(&heap);
        numbers[i] = min.value;

        int next = state_took_column(&state, min.s);

        if (next != -1) {
            min_heap_push_pop(&heap, (item) {copy[next], next % k});
        } else {
            heap_extract_min(&heap);
        }
    }
}

int state_took_column(sort_state_t *state, int index) {
    int size = state->size,
        k = state->k,
        s = index,
        *next_indices = state->next_indices;

    if (next_indices[s] >= size) {
        while (state->exhausted < k && next_indices[state->exhausted] >= state->size) {
            state->exhausted++;
        }

        if (state->exhausted == k) {
            return -1;
        }

        int next = next_indices[state->exhausted];
        next_indices[state->exhausted] += k;
        return next;
    } else {
        int next = next_indices[s];
        next_indices[s] += k;
        return s;
    }
}

/*
 * This is the merge sort from Chapter 2, modified to look only at indices
 * congruent to k modulo s. There are two very ugly and long macroses that
 * perform this unpleasant job. There's probably a nicer way to do the
 * calculation, but modular arithmetic has always been my Achilles' heel.
 */

#define FIRST(index, k, s) ((index) + (s) - (index) % (k) + ((index) % (k) <= (s) ? 0 : (k)))
#define COUNT(a, b, k, s) (((b) - (a)) / (k) + ((((s) - (a) % (k)) + (k)) % (k) < ((b) - (a)) % (k) ? 1 : 0))

void merge(int A[], int p, int q, int r, int k, int s) {
    int i, j, l;

    int n1 = COUNT(p, q, k, s);
    int n2 = COUNT(q, r, k, s);

    int L[n1];
    int R[n2];

    for (i = FIRST(p, k, s), j = 0; i < q; j++, i += k) L[j] = A[i];
    for (i = FIRST(q, k, s), j = 0; i < r; j++, i += k) R[j] = A[i];

    for(i = 0, j = 0, l = FIRST(p, k, s); l < r; l += k) {
        if (i == n1) {
            A[l] = R[j++];
        } else if (j == n2) {
            A[l] = L[i++];
        } else if (L[i] <= R[j]) {
            A[l] = L[i++];
        } else {
            A[l] = R[j++];
        }
    }
}

void merge_sort(int A[], int p, int r, int k, int s) {
    if (COUNT(p, r, k, s) > 1) {
        int q = (p + r) / 2;
        merge_sort(A, p, q, k, s);
        merge_sort(A, q, r, k, s);
        merge(A, p, q, r, k, s);
    }
}

/*
 * Finally, the min heap from exercise 6.5-3, modified to store items instead
 * of ints. When I first wrote it, I made an error in the implementation and
 * that sent me in a hour-long debugging session. C is fun.
 *
 * Also, there is a new heap operation (min_heap_push_pop) that is a faster
 * than heap_extract_min and then min_heap_insert.
 */

#define PARENT(i) ((i - 1) / 2)
#define LEFT(i)   (2 * i + 1)
#define RIGHT(i)  (2 * i + 2)

item heap_minimum(heap_t *heap) {
    return heap->elements[0];
}

void min_heapify(heap_t *heap, int i) {
    int left  = LEFT(i),
        right = RIGHT(i),
        smallest;

    if (left < heap->heap_size && heap->elements[left].value < heap->elements[i].value) {
        smallest = left;
    } else {
        smallest = i;
    }

    if (right < heap->heap_size && heap->elements[right].value < heap->elements[smallest].value) {
        smallest = right;
    }

    if (smallest != i) {
        item tmp = heap->elements[i];
        heap->elements[i] = heap->elements[smallest];
        heap->elements[smallest] = tmp;

        min_heapify(heap, smallest);
    }
}

item heap_extract_min(heap_t *heap) {
    if (heap->heap_size == 0) {
        fprintf(stderr, "heap underflow");
        exit(0);
    }

    item min = heap->elements[0];
    heap->elements[0] = heap->elements[heap->heap_size - 1];
    heap->heap_size--;
    min_heapify(heap, 0);

    return min;
}

void heap_decrease_key(heap_t *heap, int i, item key) {
    if (key.value > heap->elements[i].value) {
        fprintf(stderr, "new key is larger than current key");
        exit(0);
    }

    heap->elements[i].value = key.value;
    while (i > 0 && heap->elements[PARENT(i)].value > heap->elements[i].value) {
        item tmp = heap->elements[PARENT(i)];
        heap->elements[PARENT(i)] = heap->elements[i];
        heap->elements[i] = tmp;
        i = PARENT(i);
    }
}

void min_heap_insert(heap_t *heap, item key) {
    if (heap->length == heap->heap_size) {
        fprintf(stderr, "heap overflow");
        exit(0);
    }

    heap->elements[heap->heap_size].value = INT_MAX;
    heap->elements[heap->heap_size].s = key.s;
    heap->heap_size++;
    heap_decrease_key(heap, heap->heap_size - 1, key);
}

item min_heap_push_pop(heap_t *heap, item new) {
    item result = heap->elements[0];
    heap->elements[0] = new;
    min_heapify(heap, 0);
    return result;
}
