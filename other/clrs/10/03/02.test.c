#include "02.c"
#include "../../build/ext/test.h"

TEST(adding_elements) {
    list_t list;

    init_storage();

    list = cons(1, empty_list);
    list = cons(2, list);
    list = cons(3, list);

    ASSERT_EQUALS(get(list), 3);
    ASSERT_EQUALS(get(next(list)), 2);
    ASSERT_EQUALS(get(next(next(list))), 1);
    ASSERT_EQUALS(get(next(next(next(list)))), empty_list);
}

TEST(removing_elements) {
    list_t list, freed;

    init_storage();

    list = cons(1, empty_list);
    list = freed = cons(2, list);
    list = cons(3, list);

    delete(freed);

    ASSERT_EQUALS(get(list), 3);
    ASSERT_EQUALS(get(next(list)), 1);
    ASSERT_EQUALS(get(next(next(list))), empty_list);
    ASSERT_EQUALS(free_list, freed);
}

TEST(removing_and_adding) {
    list_t list, freed;

    init_storage();

    list = cons(1, empty_list);
    list = freed = cons(2, list);
    list = cons(3, list);

    delete(freed);

    list = cons(4, list);

    ASSERT_EQUALS(get(list), 4);
    ASSERT_EQUALS(get(next(list)), 3);
    ASSERT_EQUALS(get(next(next(list))), 1);
    ASSERT_EQUALS(get(next(next(next(list)))), empty_list);
    ASSERT_EQUALS(free_list, -1);
}
