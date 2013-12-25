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

node_t *search(list_t *list, int key) {
    node_t *node = list->nil.next;

    // The trick from exercise 10.2.4
    list->nil.key = key;
    while (node->key != key) {
        node = node->next;
    }

    if (node == &(list->nil)) {
        return NULL;
    } else {
        return node;
    }
}

void delete(list_t *list, int key) {
    node_t *node = &(list->nil);

    while (node->next != &(list->nil)) {
        if (node->next->key == key) {
            node_t *to_be_deleted = node->next;
            node->next = node->next->next;
            free(to_be_deleted);
        } else {
            node = node->next;
        }
    }
}
