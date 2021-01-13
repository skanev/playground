#include "04.c"
#include "../../build/ext/test.h"

TEST(insertion) {
    hash_t *hash = make_hash();

    insert(hash, 1);
    ASSERT_NOT_NULL(search(hash, 1));
    ASSERT_NULL(search(hash, 2));
}

TEST(deletion) {
    hash_t *hash = make_hash();

    insert(hash, 1);
    delete(hash, 1);

    ASSERT_NULL(search(hash, 1));
}

TEST(deleting_second_in_chain) {
    hash_t *hash = make_hash();

    insert(hash, 1);
    insert(hash, 11);

    delete(hash, 11);

    ASSERT_NULL(search(hash, 11));
    ASSERT_NOT_NULL(search(hash, 1));
}

TEST(deleting_first_in_chain) {
    hash_t *hash = make_hash();

    insert(hash, 1);
    insert(hash, 11);

    delete(hash, 1);

    ASSERT_NULL(search(hash, 1));
    ASSERT_NOT_NULL(search(hash, 11));
}

TEST(deleting_duplicates) {
}

TEST(inserting_duplicates) {
    hash_t *hash = make_hash();

    insert(hash, 1);
    insert(hash, 1);

    ASSERT_NOT_NULL(search(hash, 1));

    delete(hash, 1);
    ASSERT_NULL(search(hash, 1));
}

TEST(inserting_colisions) {
    hash_t *hash = make_hash();

    insert(hash, 1);
    insert(hash, 11);

    ASSERT_NOT_NULL(search(hash, 1));
    ASSERT_NOT_NULL(search(hash, 11));
}

TEST(inserting_in_foreign_slots) {
    hash_t *hash = make_hash();

    insert(hash, 1);
    insert(hash, 11);
    insert(hash, 10);

    ASSERT_NOT_NULL(search(hash, 1));
    ASSERT_NOT_NULL(search(hash, 11));
    ASSERT_NOT_NULL(search(hash, 10));
}

TEST(complicated_example) {
    hash_t *hash;
    hash = make_hash();

    insert(hash, 1);
    insert(hash, 11);
    insert(hash, 21);
    insert(hash, 31);

    insert(hash, 3);
    insert(hash, 13);
    insert(hash, 23);

    insert(hash, 5);
    insert(hash, 15);

    ASSERT_NOT_NULL(search(hash, 1));
    ASSERT_NOT_NULL(search(hash, 11));
    ASSERT_NOT_NULL(search(hash, 21));
    ASSERT_NOT_NULL(search(hash, 31));

    ASSERT_NOT_NULL(search(hash, 3));
    ASSERT_NOT_NULL(search(hash, 13));
    ASSERT_NOT_NULL(search(hash, 23));

    ASSERT_NOT_NULL(search(hash, 5));
    ASSERT_NOT_NULL(search(hash, 15));
}
