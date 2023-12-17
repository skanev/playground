#include "06.debug.c"
#include "../../build/ext/test.h"

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

void randomized_numbers(int seed, int max, int target[], int count) {
    bool used[max];

    for (int i = 0; i < max; i++) used[i] = false;

    srand(seed);

    for (int i = 0; i < count; i++) {
        while (true) {
            int number = rand() % (max - 1);

            if (!used[number]) {
                used[number] = true;
                target[i] = number + 1;
                break;
            }
        }
    }
}

int cmp(const void *pa, const void *pb) {
    int *a = (int *) pa;
    int *b = (int *) pb;
    return *a - *b;
}

#define M 30
#define L 100

int buffer[M];
int count;

void reset_buffer() {
    memset(buffer, 0, sizeof(int) * M);
    count = 0;
}

void append_to_buffer(node_t *node) {
    buffer[count++] = node->key;
}

void check_successor_invariant(tree_t *tree) {
    reset_buffer();
    int n = size(tree->root);
    successor_walk(tree->root, append_to_buffer);
    ASSERT_EQUALS(n, count);

    for (int i = 1; i < n; i++) {
        ASSERT_TRUE(buffer[i - 1] < buffer[i]);
    }
}

TEST(inserting) {
    int numbers[M];
    randomized_numbers(0, 100, numbers, M);

    tree_t *tree = make_tree();

    for (int i = 0; i < M; i++) {
        insert(tree, numbers[i]);
    }

    for (int i = 0; i < M; i++) {
        ASSERT_NOT_NULL(search(tree, numbers[i]));
    }

    check_successor_invariant(tree);
}

TEST(inorder_walk) {
    int numbers[M];
    randomized_numbers(0, 100, numbers, M);

    int sorted[M];
    memcpy(sorted, numbers, M * sizeof(int));
    qsort(sorted, M, sizeof(int), cmp);

    tree_t *tree = make_tree();

    for (int i = 0; i < M; i++) {
        insert(tree, numbers[i]);
    }

    reset_buffer();
    inorder_walk(tree->root, append_to_buffer);

    ASSERT_SAME_ARRAYS_S(buffer, sorted, M);
}

TEST(randomized_successor_walk) {
    int numbers[M];
    int sorted[M];

    for (int s = 0; s < 100; s++) {
        randomized_numbers(s, 100, numbers, M);

        memcpy(sorted, numbers, M * sizeof(int));
        qsort(sorted, M, sizeof(int), cmp);

        tree_t *tree = make_tree();

        for (int i = 0; i < M; i++) {
            insert(tree, numbers[i]);
        }

        reset_buffer();
        successor_walk(tree->root, append_to_buffer);

        ASSERT_SAME_ARRAYS_S(buffer, sorted, M);
    }
}

TEST(deleting_case_a) {
    tree_t *tree = make_tree();

    insert(tree, 5);
    insert(tree, 10);
    insert(tree, 13);
    insert(tree, 14);
    insert(tree, 12);

    delete_tree(tree, search(tree, 10));

    ASSERT_NULL(search(tree, 10));
    ASSERT_NOT_NULL(search(tree, 5));
    ASSERT_NOT_NULL(search(tree, 12));
    ASSERT_NOT_NULL(search(tree, 13));
    ASSERT_NOT_NULL(search(tree, 14));

    check_successor_invariant(tree);
}

TEST(deleting_case_a_2) {
    tree_t *tree = make_tree();

    insert(tree, 5);
    insert(tree, 10);

    delete_tree(tree, search(tree, 10));

    ASSERT_NULL(search(tree, 10));
    ASSERT_NOT_NULL(search(tree, 5));

    check_successor_invariant(tree);
}

TEST(deleting_case_b) {
    tree_t *tree = make_tree();

    insert(tree, 15);
    insert(tree, 10);
    insert(tree, 7);
    insert(tree, 8);
    insert(tree, 6);

    delete_tree(tree, search(tree, 10));

    ASSERT_NULL(search(tree, 10));
    ASSERT_NOT_NULL(search(tree, 15));
    ASSERT_NOT_NULL(search(tree, 7));
    ASSERT_NOT_NULL(search(tree, 6));
    ASSERT_NOT_NULL(search(tree, 8));

    check_successor_invariant(tree);
}


TEST(deleting_case_c) {
    tree_t *tree = make_tree();

    insert(tree, 5);
    insert(tree, 10);
    insert(tree, 7);
    insert(tree, 13);

    delete_tree(tree, search(tree, 10));

    ASSERT_NOT_NULL(search(tree, 5));
    ASSERT_NOT_NULL(search(tree, 7));
    ASSERT_NOT_NULL(search(tree, 13));

    check_successor_invariant(tree);
}

TEST(deleting_case_d) {
    tree_t *tree = make_tree();

    insert(tree, 7);
    insert(tree, 5);
    insert(tree, 3);
    insert(tree, 10);
    insert(tree, 8);
    insert(tree, 15);
    insert(tree, 19);
    insert(tree, 11);
    insert(tree, 12);

    delete_tree(tree, search(tree, 10));

    ASSERT_NULL(search(tree, 10));
    ASSERT_NOT_NULL(search(tree, 7));
    ASSERT_NOT_NULL(search(tree, 5));
    ASSERT_NOT_NULL(search(tree, 3));
    ASSERT_NOT_NULL(search(tree, 8));
    ASSERT_NOT_NULL(search(tree, 15));
    ASSERT_NOT_NULL(search(tree, 19));
    ASSERT_NOT_NULL(search(tree, 11));
    ASSERT_NOT_NULL(search(tree, 12));

    check_successor_invariant(tree);
}

TEST(deleting_in_random_trees) {
    int numbers[M];
    int to_delete = 12;

    for (int s = 0; s < 100; s++) {
        bool removed[L];

        randomized_numbers(s, L, numbers, M);

        tree_t *tree = make_tree();

        for (int i = 0; i < L; i++) removed[i] = false;
        for (int i = 0; i < M; i++) insert(tree, numbers[i]);

        for (int i = 0; i < to_delete; i++) {
            int index = rand() % M;
            int value = numbers[index];

            if (!removed[value]) {
                removed[value] = true;
                ASSERT_NOT_NULL(search(tree, value));
                delete_tree(tree, search(tree, value));
            }
        }

        check_successor_invariant(tree);

        for (int i = 0; i < M; i++) {
            if (removed[numbers[i]]) {
                ASSERT_NULL(search(tree, numbers[i]));
            } else {
                ASSERT_NOT_NULL(search(tree, numbers[i]));
            }
        }

    }
}
