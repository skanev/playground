#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>

typedef struct {
    int i;
    int j;
} cell;

typedef struct {
    int *elements;
    int m;
    int n;
} tableau_t;

cell up(cell c) {
    cell result = {c.i - 1, c.j};
    return result;
}

cell down(cell c) {
    cell result = {c.i + 1, c.j};
    return result;
}

cell left(cell c) {
    cell result = {c.i, c.j - 1};
    return result;
}

cell right(cell c) {
    cell result = {c.i, c.j + 1};
    return result;
}

cell make_cell(int i, int j) {
    cell result = {i, j};
    return result;
}

bool within(tableau_t *tableau, cell c) {
    return (c.i >= 0 && c.j >= 0 && c.i < tableau->m && c.j < tableau->n);
}

int get(tableau_t *tableau, cell c) {
    int index = c.i * tableau->n + c.j;
    return tableau->elements[index];
}

void set(tableau_t *tableau, cell c, int value) {
    int index = c.i * tableau->n + c.j;
    tableau->elements[index] = value;
}

void init_empty_tableau(tableau_t *tableau) {
    for (int i = 0; i < tableau->m * tableau-> n; i++) {
        tableau->elements[i] = INT_MAX;
    }
}

int extract_min(tableau_t *tableau) {
    int min, new;
    cell current = {0, 0},
         next;

    new = INT_MAX;
    min = get(tableau, current);

    set(tableau, current, INT_MAX);

    while (true) {
        int smallest;
        cell d = down(current);
        cell r = right(current);

        if (within(tableau, d) && get(tableau, d) < new) {
            next = d;
            smallest = get(tableau, next);
        } else {
            smallest = new;
        }

        if (within(tableau, r) && get(tableau, r) < smallest) {
            next = r;
            smallest = get(tableau, next);
        }

        if (new == smallest) {
            set(tableau, current, new);
            break;
        }

        set(tableau, current, smallest);
        current = next;
    }

    return min;
}

void insert(tableau_t *tableau, int key) {
    cell current = make_cell(tableau->m - 1, tableau->n - 1),
         next;

    if (get(tableau, current) != INT_MAX) {
        fprintf(stderr, "tableau is full\n");
        exit(0);
    }

    while (true) {
        int largest;
        cell u = up(current);
        cell l = left(current);

        if (within(tableau, u) && get(tableau, u) > key) {
            next = u;
            largest = get(tableau, next);
        } else {
            largest = key;
        }

        if (within(tableau, l) && get(tableau, l) > largest) {
            next = l;
            largest = get(tableau, next);
        }

        if (key == largest) {
            set(tableau, current, key);
            break;
        }

        set(tableau, current, largest);
        current = next;
    }
}

void sort(int *array, int size_sqrt) {
    int elements[size_sqrt * size_sqrt];
    tableau_t tableau = {elements, size_sqrt, size_sqrt};

    init_empty_tableau(&tableau);

    for (int i = 0; i < size_sqrt * size_sqrt; i++) {
        insert(&tableau, array[i]);
    }

    for (int i = 0; i < size_sqrt * size_sqrt; i++) {
        int next = extract_min(&tableau);
        array[i] = next;
    }
}

bool find(tableau_t *tableau, int key) {
    cell c = {tableau->m - 1, 0};

    while (within(tableau, c)) {
        int value = get(tableau, c);

        if (value == key) {
            return true;
        } else if (value > key) {
            c = up(c);
        } else {
            c = right(c);
        }
    }

    return false;
}
