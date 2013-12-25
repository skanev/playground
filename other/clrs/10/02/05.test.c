#include "05.c"
#include "../../build/ext/test.h"

TEST(inserting_and_searching) {
    list_t list;
    init_list(&list);

    insert(&list, 1);
    insert(&list, 2);
    insert(&list, 3);

    ASSERT_TRUE(search(&list, 1) != NULL);
    ASSERT_TRUE(search(&list, 2) != NULL);
    ASSERT_TRUE(search(&list, 3) != NULL);
    ASSERT_TRUE(search(&list, 4) == NULL);

    destroy_list(&list);
}

TEST(deleting) {
    list_t list;
    init_list(&list);

    insert(&list, 1);
    insert(&list, 2);
    insert(&list, 3);

    ASSERT_TRUE(search(&list, 1) != NULL);

    delete(&list, 2);

    ASSERT_TRUE(search(&list, 2) == NULL);

    delete(&list, 3);

    ASSERT_TRUE(search(&list, 3) == NULL);
    ASSERT_TRUE(search(&list, 1) != NULL);

    destroy_list(&list);
}

TEST(deleting_an_element_inserted_multiple_times) {
    list_t list;
    init_list(&list);

    insert(&list, 1);
    insert(&list, 2);
    insert(&list, 2);
    insert(&list, 3);
    insert(&list, 2);

    ASSERT_TRUE(search(&list, 2) != NULL);

    delete(&list, 2);

    ASSERT_TRUE(search(&list, 2) == NULL);

    destroy_list(&list);
}
