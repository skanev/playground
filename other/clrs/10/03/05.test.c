#include "05.c"
#include "../../build/ext/test.h"

TEST(compactify) {
    list_t list;

    init_storage();

    list = cons(0, empty_list);
    list = cons(1, list);
    list = cons(2, list);
    list = cons(3, list);
    list = cons(4, list);
    list = cons(5, list);
    list = cons(6, list);
    list = cons(7, list);
    list = cons(8, list);

    delete(2);
    delete(3);
    delete(5);
    delete(6);

    list = compatify_list(list);

    ASSERT_EQUALS(get(list), 8);
    ASSERT_EQUALS(get(next_obj(list)), 7);
    ASSERT_EQUALS(get(next_obj(next_obj(list))), 4);
    ASSERT_EQUALS(get(next_obj(next_obj(next_obj(list)))), 1);
    ASSERT_EQUALS(get(next_obj(next_obj(next_obj(next_obj(list))))), 0);
    ASSERT_EQUALS(get(next_obj(next_obj(next_obj(next_obj(next_obj(list)))))), -1);
    ASSERT_EQUALS(free_list, 5);
}
