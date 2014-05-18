// unit_tests.c
// Gathers all unit tests in one place.

#include "unit_tests.h"
#include "test_suite.h"

/*******************
 * Internal Macros *
 *******************/

// Expands into a function that will build the test suite with the given name
// when called.
#define DEFINE_TEST_SUITE_BUILDER(NAME, TESTS...) \
  test_suite * build_test_suite_ ## NAME(void) { \
    unit_test tests[] = TESTS; \
    test_suite *ts = create_test_suite(#NAME); \
    int index = 0; \
    unit_test next = NULL; \
    next = tests[index]; \
    while (next != NULL) { \
      ts_add_test(ts, next); \
      index += 1; \
      next = tests[index]; \
    } \
    return ts; \
  }

// Invokes the test suite constructor defined by DEFINE_TEST_SUITE_BUILDER for
// the given test suite name.
#define INVOKE_TEST_SUITE_BUILDER_(NAME) build_test_suite_ ## NAME()
#define INVOKE_TEST_SUITE_BUILDER(NAME) INVOKE_TEST_SUITE_BUILDER_(NAME)

// Expands into a macro call that defines a builder for the most-recently-
// imported test suite header file.
#define DEFINE_IMPORTED_BUILDER_(NAME, TESTS...) \
  DEFINE_TEST_SUITE_BUILDER(NAME, TESTS)
#define DEFINE_IMPORTED_BUILDER \
  DEFINE_IMPORTED_BUILDER_(TEST_SUITE_NAME, TEST_SUITE_TESTS)

// Expands into a macro call that invokes the builder for the most-recently-
// (re)imported test suite header file.
#define INVOKE_IMPORTED_BUILDER \
  INVOKE_TEST_SUITE_BUILDER(TEST_SUITE_NAME)

/**********************
 * Test Suite Imports *
 **********************/

#include "suites/test_list.h"
DEFINE_IMPORTED_BUILDER
#include "suites/test_queue.h"
DEFINE_IMPORTED_BUILDER
#include "suites/test_map.h"
DEFINE_IMPORTED_BUILDER
#include "suites/test_map3.h"
DEFINE_IMPORTED_BUILDER

/***********
 * Globals *
 ***********/

list * ALL_TEST_SUITES;

/*************
 * Functions *
 *************/

void setup_unit_tests(void) {
  test_suite *ts;
  ALL_TEST_SUITES = create_list();
  // Note that these #includes must be reimports or their syntax will be wrong.
  // Do data structure tests last 'cause they're slower:
  #include "suites/test_list.h"
  ts = INVOKE_IMPORTED_BUILDER;
  l_append_element(ALL_TEST_SUITES, ts);
  #include "suites/test_queue.h"
  ts = INVOKE_IMPORTED_BUILDER;
  l_append_element(ALL_TEST_SUITES, ts);
  #include "suites/test_map.h"
  ts = INVOKE_IMPORTED_BUILDER;
  l_append_element(ALL_TEST_SUITES, ts);
  #include "suites/test_map3.h"
  ts = INVOKE_IMPORTED_BUILDER;
  l_append_element(ALL_TEST_SUITES, ts);
}

void cleanup_unit_tests(void) {
  int i = 0;
  test_suite *ts = NULL;
  for (i = 0; i < l_get_length(ALL_TEST_SUITES); ++i) {
    ts = (test_suite *) l_get_item(ALL_TEST_SUITES, i);
    cleanup_test_suite(ts);
  }
  cleanup_list(ALL_TEST_SUITES);
}

void run_all_tests(void) {
  int i = 0;
  test_suite *ts = NULL;
  for (i = 0; i < l_get_length(ALL_TEST_SUITES); ++i) {
    ts = (test_suite *) l_get_item(ALL_TEST_SUITES, i);
    ts_run_tests(ts);
  }
}

void print_all_results(void) {
  int i = 0;
  test_suite *ts = NULL;
  for (i = 0; i < l_get_length(ALL_TEST_SUITES); ++i) {
    ts = (test_suite *) l_get_item(ALL_TEST_SUITES, i);
    ts_print_short_results(ts);
  }
  printf("\n");
  for (i = 0; i < l_get_length(ALL_TEST_SUITES); ++i) {
    ts = (test_suite *) l_get_item(ALL_TEST_SUITES, i);
    if (!ts_passed(ts)) {
      ts_print_detailed_results(ts);
    }
  }
}

int all_tests_passed(void) {
  int i = 0;
  test_suite *ts = NULL;
  for (i = 0; i < l_get_length(ALL_TEST_SUITES); ++i) {
    ts = (test_suite *) l_get_item(ALL_TEST_SUITES, i);
    if (!ts_passed(ts)) {
      return 0;
    }
  }
  return 1;
}

int main(int argv, char **argc) {
  int result = 1;
  setup_unit_tests();
  run_all_tests();
  print_all_results();
  if (all_tests_passed()) {
    result = 0;
  }
  cleanup_unit_tests();
  return result;
}
