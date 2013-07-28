#include "02.c"
#include "../../build/ext/test.h"

#include <stdlib.h>

#define SIZE 8
#define CHECKS 1000
#define MAX_ELEMENT_SIZE 10

void multiply(matrix C, matrix A, matrix B) {
    for (int i = 0; i < C.size; i++) {
        for (int j = 0; j < C.size; j++) {
            int result = 0;

            for (int k = 0; k < C.size; k++) {
                result += get(A, i, k) * get(B, k, j);
            }

            put(C, i, j, result);
        }
    }
}

TEST(random_matches) {
    int a[SIZE * SIZE];
    int b[SIZE * SIZE];
    int c[SIZE * SIZE];
    int d[SIZE * SIZE];

    matrix A = create_matrix(SIZE, a);
    matrix B = create_matrix(SIZE, b);
    matrix C = create_matrix(SIZE, c);
    matrix D = create_matrix(SIZE, d);

    for (unsigned seed = 0; seed < CHECKS; seed++) {
        srand(seed);

        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                put(A, i, j, rand() % MAX_ELEMENT_SIZE);
                put(B, i, j, rand() % MAX_ELEMENT_SIZE);
            }
        }

        strassen(C, A, B);
        multiply(D, A, B);

        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                ASSERT_EQUALS(get(C, i, j), get(D, i, j));
            }
        }
    }
}
