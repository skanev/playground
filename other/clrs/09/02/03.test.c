#include "03.c"
#include "../../build/ext/test.h"

#define SIZE 10
#define SEED 300

void generate_array(int *numbers, int size);

TEST(ith_order_statistic) {
    int numbers[SIZE];

    generate_array(numbers, SIZE);
    ASSERT_EQUALS(randomized_select(numbers, 0, SIZE, 0), 0);

    generate_array(numbers, SIZE);
    ASSERT_EQUALS(randomized_select(numbers, 0, SIZE, SIZE / 2), SIZE / 2);

    generate_array(numbers, SIZE);
    ASSERT_EQUALS(randomized_select(numbers, 0, SIZE, SIZE - 1), SIZE - 1);
}

void generate_array(int *numbers, int size) {
    srand(SEED);
    for (int i = 0; i < size; i++) {
        numbers[i] = i;
    }

    for (int i = size; i >= 1; i--) {
        int pos = rand() % i;
        EXCHANGE(numbers[i - 1], numbers[pos]);
    }
}
