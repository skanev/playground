#include <stdio.h>
#include <setjmp.h>
#include "debug_helpers.h"

/*
 * A small testing library I'm writing for the Introduction to Algorithms
 * study group. It will probably be expanded as we go.
 */

/*
 * State and initialization.
 */

const char *test_current_name;
const char *test_last_assert_file;
int test_last_assert_line;
int test_failures;
int test_runs;
jmp_buf test_on_fail;

void test_initialize() {
  test_current_name = "";
  test_last_assert_file = "";
  test_last_assert_line = -1;
  test_runs = 0;
  test_failures = 0;
}

/*
 * Reporting test runs
 */

void test_report_assertion_error(const char *assertion_name) {
  fprintf(stderr, "%s:%d: %s (%s)\n",
    test_current_name,
    test_last_assert_line,
    assertion_name,
    test_last_assert_file
  );
}

int test_report_results() {
  if (test_runs == 0) {
    fprintf(stderr, "No tests were ran ;(\n");
    return -1;
  }

  if (test_failures) {
    fprintf(stderr, "\n");
    printf("FAILURE %d test(s), %d failure(s)\n", test_runs, test_failures);
    return 1;
  }

  printf("OK %d test(s)\n", test_runs);
  return 0;
}

/*
 * Running a test
 */

void run_test(const char *name, void (*code)()) {
  test_current_name = name;
  test_last_assert_file = "";
  test_last_assert_line = -1;

  test_runs++;

  if (setjmp(test_on_fail)) {
    test_failures++;
  } else {
    code();
  }
}

void abort_test() {
  longjmp(test_on_fail, 1);
}

/*
 * Assertions
 */

#define ASSERT_SAME_ARRAYS(a, b) \
  test_last_assert_file = __FILE__; \
  test_last_assert_line = __LINE__; \
  assert_same_arrays(a, b, sizeof(a) / sizeof(a[1]), sizeof(b) / sizeof(b[1]))

#define ASSERT_SAME_ARRAYS_S(a, b, s) \
  test_last_assert_file = __FILE__; \
  test_last_assert_line = __LINE__; \
  assert_same_arrays(a, b, s, s)

void assert_same_arrays(int a[], int b[], int l1, int l2) {
  if (l1 != l2)
    goto report_failure;

  for (int i = 0; i < l1; i++)
    if (a[i] != b[i])
      goto report_failure;

  return;

report_failure:
  test_report_assertion_error("assert_same_arrays");
  fprintf(stderr, "    expected ");
  fprint_array(stderr, a, l1);
  fprintf(stderr, "\n    to equal ");
  fprint_array(stderr, b, l2);
  fprintf(stderr, "\n");
  abort_test();
}

#define ASSERT_EQUALS(a, b) \
  test_last_assert_file = __FILE__; \
  test_last_assert_line = __LINE__; \
  assert_equals(a, b)

void assert_equals(int a, int b) {
  if (a != b) {
    test_report_assertion_error("assert_equals");
    fprintf(stderr, "    expected %d\n", a);
    fprintf(stderr, "    to equal %d\n", b);
    fprintf(stderr, "\n");
    abort_test();
  }
}

/*
 * Defining tests
 */

#define TEST(NAME) \
  void NAME##_test_code(); \
  void run_##NAME() { run_test(#NAME, &NAME##_test_code); } \
  void NAME##_test_code()
