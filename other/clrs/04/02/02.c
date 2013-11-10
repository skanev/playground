#include <alloca.h>
#include <stdio.h>

// The matrix representation. One structure to fit both a matrix and a
// submatrix.

typedef struct {
    int x;
    int y;
    int size;
    int original_size;
    int *data;
} matrix;

// Functions to index matrices

int get(matrix m, int x, int y) {
    return m.data[m.original_size * (m.x + x) + m.y + y];
};

void put(matrix m, int x, int y, int value) {
    m.data[m.original_size * (m.x + x) + m.y + y] = value;
};

// Matrix building

matrix create_matrix(int size, int *data) {
    matrix result;
    result.x = 0;
    result.y = 0;
    result.size = size;
    result.original_size = size;
    result.data = data;

    return result;
}

matrix submatrix(matrix A, int x, int y, int size) {
    matrix result;
    result.x = A.x + x;
    result.y = A.y + y;
    result.size = size;
    result.original_size = A.original_size;
    result.data = A.data;
    return result;
}

#define INIT_ON_STACK(m_, size_) \
    m_.x = 0; \
    m_.y = 0; \
    m_.size = size_; \
    m_.original_size = size_; \
    m_.data = alloca(size_ * size_ * sizeof(int));

// Adding and subtracting matrices

void plus(matrix C, matrix A, matrix B) {
    for (int i = 0; i < C.size; i++) {
        for (int j = 0; j < C.size; j++) {
            put(C, i, j, get(A, i, j) + get(B, i, j));
        }
    }
}

void minus(matrix C, matrix A, matrix B) {
    for (int i = 0; i < C.size; i++) {
        for (int j = 0; j < C.size; j++) {
            put(C, i, j, get(A, i, j) - get(B, i, j));
        }
    }
}

void add(matrix T, matrix S) {
    for (int i = 0; i < T.size; i++) {
        for (int j = 0; j < T.size; j++) {
            put(T, i, j, get(T, i, j) + get(S, i, j));
        }
    }
}
void sub(matrix T, matrix S) {
    for (int i = 0; i < T.size; i++) {
        for (int j = 0; j < T.size; j++) {
            put(T, i, j, get(T, i, j) - get(S, i, j));
        }
    }
}

void zero(matrix m) {
    for (int i = 0; i < m.size; i++) {
        for (int j = 0; j < m.size; j++) {
            put(m, i, j, 0);
        }
    }
}

// A function to print matrices

void print_matrix(matrix m) {
    printf("%dx%d (+%d+%d) (%d)\n", m.size, m.size, m.x, m.y, m.original_size);
    printf("==============\n");
    for (int i = 0; i < m.size; i++) {
        for (int j = 0; j < m.size; j++) {
            printf("%4d", get(m, i, j));
        }
        printf("\n");
    }
    printf("\n");
}

// Strassen's algorithm

void strassen(matrix C, matrix A, matrix B) {
    int size = A.size,
        half = size / 2;

    if (A.size == 1) {
        put(C, 0, 0, get(A, 0, 0) * get(B, 0, 0));
    } else {
        matrix s1, s2, s3, s4, s5, s6, s7, s8, s9, s10;
        matrix p1, p2, p3, p4, p5, p6, p7;

        INIT_ON_STACK(s1, half);
        INIT_ON_STACK(s2, half);
        INIT_ON_STACK(s3, half);
        INIT_ON_STACK(s4, half);
        INIT_ON_STACK(s5, half);
        INIT_ON_STACK(s6, half);
        INIT_ON_STACK(s7, half);
        INIT_ON_STACK(s8, half);
        INIT_ON_STACK(s9, half);
        INIT_ON_STACK(s10, half);

        INIT_ON_STACK(p1, half);
        INIT_ON_STACK(p2, half);
        INIT_ON_STACK(p3, half);
        INIT_ON_STACK(p4, half);
        INIT_ON_STACK(p5, half);
        INIT_ON_STACK(p6, half);
        INIT_ON_STACK(p7, half);

        matrix a11 = submatrix(A,    0,    0, half);
        matrix a12 = submatrix(A,    0, half, half);
        matrix a21 = submatrix(A, half,    0, half);
        matrix a22 = submatrix(A, half, half, half);

        matrix b11 = submatrix(B,    0,    0, half);
        matrix b12 = submatrix(B,    0, half, half);
        matrix b21 = submatrix(B, half,    0, half);
        matrix b22 = submatrix(B, half, half, half);

        matrix c11 = submatrix(C,    0,    0, half);
        matrix c12 = submatrix(C,    0, half, half);
        matrix c21 = submatrix(C, half,    0, half);
        matrix c22 = submatrix(C, half, half, half);

        minus(s1, b12, b22);
        plus(s2,  a11, a12);
        plus(s3,  a21, a22);
        minus(s4, b21, b11);
        plus(s5,  a11, a22);
        plus(s6,  b11, b22);
        minus(s7, a12, a22);
        plus(s8,  b21, b22);
        minus(s9, a11, a21);
        plus(s10, b11, b12);

        strassen(p1, a11, s1);
        strassen(p2, s2, b22);
        strassen(p3, s3, b11);
        strassen(p4, a22, s4);
        strassen(p5, s5, s6);
        strassen(p6, s7, s8);
        strassen(p7, s9, s10);

        zero(c11);
        zero(c12);
        zero(c21);
        zero(c22);

        add(c11, p5);
        add(c11, p4);
        sub(c11, p2);
        add(c11, p6);

        add(c12, p1);
        add(c12, p2);

        add(c21, p3);
        add(c21, p4);

        add(c22, p5);
        add(c22, p1);
        sub(c22, p3);
        sub(c22, p7);
    }
}
