#include <stdlib.h>
#include <stdio.h>

typedef struct node_t {
    int key;
    struct node_t *np;
} node_t;

typedef struct {
    struct node_t *head;
    struct node_t *tail;
} list_t;

node_t *xor(node_t *left, node_t *right) {
    return (node_t *) (((unsigned long) left) ^ ((unsigned long) right));
}

void init_list(list_t *list) {
    list->head = NULL;
    list->tail = NULL;
}

void destroy_list(list_t *list) {
    node_t *prev = NULL;
    node_t *node = list->head;
    node_t *next;

    while (node) {
        next = xor(node->np, prev);
        free(node);
        prev = node;
        node = next;
    }
}

void insert(list_t *list, int key) {
    node_t *new = (node_t *) malloc(sizeof(node_t));
    new->key = key;
    new->np = xor(NULL, list->tail);

    if (list->tail) {
        list->tail->np = xor(new, xor(NULL, list->tail->np));
    }

    if (!list->head) {
        list->head = new;
    }

    list->tail = new;
}

int get(list_t *list, int index) {
    node_t *node = list->head;
    node_t *prev = NULL;
    node_t *next;

    while (index--) {
        if (!node) {
            fprintf(stderr, "Index out of bounds\n");
            exit(1);
        }
        next = xor(node->np, prev);
        prev = node;
        node = next;
    }

    return node->key;
}

node_t *search(list_t *list, int key) {
    node_t *node = list->head;
    node_t *prev = NULL;
    node_t *next;

    while (node) {
        if (node->key == key) {
            return node;
        }

        next = xor(node->np, prev);
        prev = node;
        node = next;
    }

    return NULL;
}

void delete(list_t *list, int key) {
    node_t *node = list->head;
    node_t *prev = NULL;
    node_t *next;

    while (node) {
        if (node->key == key) {
            next = xor(node->np, prev);

            if (prev) {
                prev->np = xor(xor(prev->np, node), next);
            } else {
                list->head = next;
            }

            if (next) {
                next->np = xor(xor(next->np, node), prev);
            } else {
                list->tail = prev;
            }

            node = next;
        } else {
            next = xor(node->np, prev);
            prev = node;
            node = next;
        }
    }
}

void reverse(list_t *list) {
    node_t *tmp;
    tmp = list->head;
    list->head = list->tail;
    list->tail = tmp;
}
