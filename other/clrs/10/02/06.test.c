#include "08.c"
#include "../../build/ext/test.h"

TEST(inserting_and_searching) {
    list_t list;
    init_list(&list);

    insert(&list, 1);
    insert(&list, 2);
    insert(&list, 3);

    ASSERT_TRUE(search(&list, 0) == NULL);
    ASSERT_TRUE(search(&list, 1) != NULL);
    ASSERT_TRUE(search(&list, 2) != NULL);
    ASSERT_TRUE(search(&list, 3) != NULL);
    ASSERT_TRUE(search(&list, 4) == NULL);

    destroy_list(&list);
}

TEST(deleting) {
    list_t list;
    init_list(&list);

    insert(&list, 2);
    insert(&list, 1);
    insert(&list, 2);
    insert(&list, 3);
    insert(&list, 2);
    insert(&list, 2);

    delete(&list, 2);

    ASSERT_TRUE(search(&list, 2) == NULL);

    destroy_list(&list);
}

TEST(reversing) {
    list_t list;
    init_list(&list);

    insert(&list, 1);
    insert(&list, 2);
    insert(&list, 3);

    reverse(&list);

    ASSERT_EQUALS(get(&list, 0), 3);
    ASSERT_EQUALS(get(&list, 1), 2);
    ASSERT_EQUALS(get(&list, 2), 1);

    destroy_list(&list);
}
