#include "07.c"
#include "../../build/ext/test.h"

TEST(inserting_and_searching) {
    list_t list;
    init_list(&list);

    insert(&list, 1);
    insert(&list, 2);
    insert(&list, 3);

    reverse(&list);

    ASSERT_EQUALS(list.nil.next->key,             1);
    ASSERT_EQUALS(list.nil.next->next->key,       2);
    ASSERT_EQUALS(list.nil.next->next->next->key, 3);

    destroy_list(&list);
}

