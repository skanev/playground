#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define SIZE 10
#define HASH(v) (v % 10)

// --- Types and internals --------------------------------------------------

typedef int value_t;

struct element_t;

typedef struct element_t {
    char free;
    union {
        struct {
            struct element_t *prev;
            struct element_t *next;
        } empty;

        struct {
            value_t value;
            struct element_t *next;
        } used;
    };
} element_t;

typedef struct {
    element_t buckets[SIZE];
    element_t free_list;
} hash_t;

// --- Helpers --------------------------------------------------------------

int hash_value(value_t value) {
    return HASH(value);
}

void remove_from_free_list(hash_t *hash, element_t *element) {
    element->empty.prev->empty.next = element->empty.next;
    element->empty.next->empty.prev = element->empty.prev;
    element->free = 0;
}

void return_to_free_list(hash_t *hash, element_t *element) {
    element_t *sentinel = &(hash->free_list);

    element->free = 1;
    element->empty.next = sentinel->empty.next;
    element->empty.prev = sentinel;

    sentinel->empty.next = element;
}

element_t *allocate(hash_t *hash) {
    element_t *element = hash->free_list.used.next;

    assert(element != &(hash->free_list));

    remove_from_free_list(hash, element);

    return element;
}

void reallocate(hash_t *hash, element_t *element) {
    int index = hash_value(element->used.value);

    element_t *location = &(hash->buckets[index]);

    assert(!location->free);

    element_t *new = allocate(hash);
    new->used.value = element->used.value;
    new->used.next = element->used.next;

    while (location->used.next != element) {
        location = location->used.next;

        assert(location);
        assert(!location->free);
    }

    location->used.next = new;
}

// --- Public interface -----------------------------------------------------

hash_t *make_hash() {
    hash_t *hash = malloc(sizeof(hash_t));

    hash->free_list.empty.next = hash->buckets;
    hash->free_list.empty.prev = hash->buckets + SIZE;

    element_t *current = &(hash->free_list);
    current->free = 1;

    for (int i = 0; i < SIZE; i++) {
        element_t *next = &(hash->buckets[i]);

        next->free = 1;
        next->empty.prev = current;

        current->empty.next = next;

        current = next;
    }

    current->empty.next = &(hash->free_list);
    hash->free_list.empty.prev = current;

    return hash;
}

element_t *search(hash_t *hash, value_t value) {
    int index = hash_value(value);

    element_t *element = &(hash->buckets[index]);

    while (element && !element->free) {
        if (element->used.value == value) {
            return element;
        }

        element = element->used.next;
    }

    return NULL;
}

void insert(hash_t *hash, value_t value) {
    int index = hash_value(value);

    element_t *element = &(hash->buckets[index]);

    if (element->free) {
        remove_from_free_list(hash, element);

        element->used.value = value;
        element->used.next = NULL;
    } else if (hash_value(element->used.value) == index) {
        element_t *new = allocate(hash);

        new->used.value = value;
        new->used.next = element->used.next;
        element->used.next = new;
    } else {
        reallocate(hash, element);

        element->used.value = value;
        element->used.next = NULL;
    }
}

void delete(hash_t *hash, value_t value) {
    int index = hash_value(value);

    element_t *head = &(hash->buckets[index]);

    if (head->free || hash_value(head->used.value) != index) {
        return;
    }

    while (head->used.value == value) {
        element_t *next = head->used.next;

        if (next) {
            assert(!next->free);
            assert(hash_value(next->used.value) == index);

            head->used.value = next->used.value;
            head->used.next = next->used.next;
            return_to_free_list(hash, next);
        } else {
            return_to_free_list(hash, head);
            return;
        }
    }

    element_t *element = head;

    while (element->used.next) {
        element_t *next = element->used.next;

        assert(!next->free);
        assert(hash_value(next->used.value) == index);

        if (next->used.value == value) {
            element->used.next = next->used.next;
            return_to_free_list(hash, next);
        } else {
            element = next;
        }
    }
}

// --- Debug ----------------------------------------------------------------

void print_hash(hash_t *hash) {
    int free_slots = 0;
    element_t *item = hash->free_list.empty.next;

    while (item != &(hash->free_list)) {
        item = item->empty.next;
        free_slots++;
    }

    printf("\nfree slots in linked list: %d\n", free_slots);

    free_slots = 0;

    for (int i = 0; i < SIZE; i++) {
        if (hash->buckets[i].free) free_slots++;
    }

    printf("free slots in hash array:  %d\n\n", free_slots);

    for (int i = 0; i < SIZE; i++) {
        element_t *element = &(hash->buckets[i]);

        printf("    +------+\n");
        printf(" %-2d | ", i);

        if (element->free) {
            printf("     |");
        } else {
            int foreign = hash_value(element->used.value) != i;
            printf("%1s %2d |", (foreign ? "/" : " "), element->used.value);

            if (!foreign) {
                while (element->used.next) {
                    printf(" -> ");
                    element = element->used.next;

                    if (element->free) {
                        printf("!!FREE\n");
                        break;
                    }

                    printf("%2d", element->used.value);
                }
            }
        }
        printf("\n");
    }

    printf("    +------+\n");
    printf("\n\n");
}
