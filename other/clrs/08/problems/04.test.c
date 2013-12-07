#include "04.c"
#include "../../build/ext/test.h"

#define SIZE 10000
#define RANGE 1000000
#define SEED 300

void generate_jugs(int *red, int *blue);
void assert_paired(int *red, int *blue);

TEST(quadratic_pairing) {
    int red_jugs[SIZE];
    int blue_jugs[SIZE];

    generate_jugs(red_jugs, blue_jugs);

    quadratic_pair(red_jugs, blue_jugs, SIZE);

    assert_paired(red_jugs, blue_jugs);
}

TEST(quick_pairing) {
    int red_jugs[SIZE];
    int blue_jugs[SIZE];

    generate_jugs(red_jugs, blue_jugs);

    quick_pair(red_jugs, blue_jugs, 0, SIZE);

    assert_paired(red_jugs, blue_jugs);
}

void generate_jugs(int *red, int *blue) {
    srand(SEED);
    for (int i = 0; i < SIZE; i++) {
        red[i] = rand() % RANGE;
    }

    for (int i = 0; i < SIZE; i++) {
        blue[i] = red[i];
    }

    for (int i = SIZE; i >= 1; i--) {
        int pos = rand() % i;
        EXCHANGE(blue[i - 1], blue[pos]);
    }
}

void assert_paired(int *red, int *blue) {
    for (int i = 0; i < SIZE; i++) {
        if (red[i] != blue[i]) {
            FAIL("Not paired at index %d: %d != %d", i, red[i], blue[i]);
        }
    }
}
