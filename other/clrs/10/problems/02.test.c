#include "02.c"
#include "../../build/ext/test.h"

heap1 *build1(int first, ...) {
    heap1 *result = make_heap1();
    va_list marker;
    int i = first;

    va_start(marker, first);
    while (i != -1) {
        insert1(result, i);
        i = va_arg(marker, int);
    }
    va_end(marker);

    return result;
}

heap2 *build2(int first, ...) {
    heap2 *result = make_heap2();
    va_list marker;
    int i = first;

    va_start(marker, first);
    while (i != -1) {
        insert2(result, i);
        i = va_arg(marker, int);
    }
    va_end(marker);

    return result;
}

TEST(heap1) {
    heap1 *h1 = build1(4, 6, 1, -1);
    heap1 *h2 = build1(2, 5, 3, -1);

    ASSERT_EQUALS(minimum1(h1), 1);
    ASSERT_EQUALS(minimum1(h2), 2);

    heap1 *h3 = union1(h2, h1);
    ASSERT_EQUALS(extract_min1(h3), 1);
    ASSERT_EQUALS(extract_min1(h3), 2);
    ASSERT_EQUALS(extract_min1(h3), 3);
}

TEST(heap2) {
    heap2 *h1 = build2(4, 2, 6, 1, -1);
    heap2 *h2 = build2(2, 5, 3, 3, -1);

    ASSERT_EQUALS(minimum2(h2), 2);
    ASSERT_EQUALS(minimum2(h1), 1);

    heap2 *h3 = union2(h2, h1);
    ASSERT_EQUALS(extract_min2(h3), 1);
    ASSERT_EQUALS(extract_min2(h3), 2);
    ASSERT_EQUALS(extract_min2(h3), 3);
    ASSERT_EQUALS(extract_min2(h3), 4);
}
