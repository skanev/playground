#include <stdlib.h>

typedef struct node_t {
    int key;
    struct node_t *next;
} node_t;

typedef struct {
    struct node_t nil;
} list_t;

void init_list(list_t *list) {
    list->nil.key = 0;
    list->nil.next = &(list->nil);
}

void destroy_list(list_t *list) {
    node_t *node = list->nil.next;
    node_t *next;

    while (node != &(list->nil)) {
        next = node->next;
        free(node);
        node = next;
    }
}

void insert(list_t *list, int key) {
    node_t *new = (node_t *) malloc(sizeof(node_t));
    new->key = key;
    new->next = list->nil.next;
    list->nil.next = new;
}

void reverse(list_t *list) {
    node_t *prev = &(list->nil);
    node_t *node = list->nil.next;
    node_t *next;

    while (node != &(list->nil)) {
        next = node->next;
        node->next = prev;
        prev = node;
        node = next;
    }

    list->nil.next = prev;
}
