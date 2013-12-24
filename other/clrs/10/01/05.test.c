#include "05.c"
#include "../../build/ext/test.h"

TEST(using_as_a_stack) {
    deque_t deque;

    init_deque(&deque);
    push(&deque, 1);
    push(&deque, 2);
    push(&deque, 3);

    ASSERT_FALSE(is_empty(&deque));
    ASSERT_EQUALS(pop(&deque), 3);
    ASSERT_EQUALS(pop(&deque), 2);
    ASSERT_EQUALS(pop(&deque), 1);
    ASSERT_TRUE(is_empty(&deque));

    push(&deque, 4);
    ASSERT_FALSE(is_empty(&deque));
    ASSERT_EQUALS(pop(&deque), 4);
    ASSERT_TRUE(is_empty(&deque));
}

TEST(using_as_queue) {
    deque_t deque;

    init_deque(&deque);
    push(&deque, 1);
    push(&deque, 2);
    push(&deque, 3);

    ASSERT_FALSE(is_empty(&deque));
    ASSERT_EQUALS(shift(&deque), 1);
    ASSERT_EQUALS(shift(&deque), 2);
    ASSERT_EQUALS(shift(&deque), 3);
    ASSERT_TRUE(is_empty(&deque));
}

TEST(unshifting) {
    deque_t deque;

    init_deque(&deque);
    unshift(&deque, 1);
    unshift(&deque, 2);
    unshift(&deque, 3);

    ASSERT_FALSE(is_empty(&deque));
    ASSERT_EQUALS(pop(&deque), 1);
    ASSERT_EQUALS(pop(&deque), 2);
    ASSERT_EQUALS(pop(&deque), 3);
    ASSERT_TRUE(is_empty(&deque));
}

TEST(wrapping_around_on_push) {
    deque_t deque;
    int i;

    init_deque(&deque);

    for (i = 0; i < MAX_SIZE; i++) {
        push(&deque, i);
    }

    shift(&deque);
    push(&deque, i);

    ASSERT_EQUALS(deque.items[0], i);
}

TEST(wrapping_around_on_shift) {
    deque_t deque;
    int i;

    init_deque(&deque);

    for (i = 0; i < MAX_SIZE - 1; i++) {
        unshift(&deque, i);
    }

    unshift(&deque, i);

    ASSERT_EQUALS(deque.items[0], i);
}
