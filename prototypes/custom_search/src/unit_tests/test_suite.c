// test_suite.c
// A simple unit testing harness.

//#include <stdlib.h>
#include <stdio.h>
//#include <assert.h>
//#include <errno.h>

#include "test_suite.h"

#include "datatypes/list.h"

/*************
 * Functions *
 *************/

test_suite *create_test_suite(char const * const name) {
  test_suite *ts = (test_suite *) malloc(sizeof(test_suite));
  ts->name = name;
  ts->tests = create_list();
  ts->results = create_list();
  ts->passed = 0;
  ts->failed = 0;
  return ts;
}

void cleanup_test_suite(test_suite *ts) {
  cleanup_list(ts->tests);
  cleanup_list(ts->results);
  free(ts);
}

void ts_add_test(test_suite *ts, unit_test test) {
  l_append_element(ts->tests, (void *) test);
  l_append_element(ts->results, (void *) 0);
}

int ts_passed(test_suite *ts) {
  return ts->passed == l_get_length(ts->tests) && ts->failed == 0;
}

void ts_print_short_results(test_suite *ts) {
  printf("Test suite '%s' summary:", ts->name);
  if (ts_passed(ts)) {
    printf(" All tests passed.\n");
  } else {
    printf(
      "\n  %d/%zu tests passed, %d/%zu tests FAILED.\n",
      ts->passed, l_get_length(ts->tests),
      ts->failed, l_get_length(ts->tests)
    );
  }
}

void ts_print_detailed_results(test_suite *ts) {
  int i = 0;
  size_t result = 0;
  printf("Test suite '%s' details:\n", ts->name);
  for (i = 0; i < l_get_length(ts->tests); ++i) {
    result = (size_t) l_get_item(ts->results, i);
    if (result == 0) {
      printf("  Test #%d: passed.\n", i);
    } else {
      printf("  Test #%d: FAILED: %zu.\n", i, result);
    }
  }
  if (ts_passed(ts)) {
    printf("All tests passed.\n");
  } else {
    printf(
      "%d/%zu tests passed, %d/%zu tests FAILED.\n",
      ts->passed, l_get_length(ts->tests),
      ts->failed, l_get_length(ts->tests)
    );
  }
}

void ts_run_tests(test_suite *ts) {
  int i = 0;
  size_t result = 0;
  unit_test test;
  ts->passed = 0;
  ts->failed = 0;
  for (i = 0; i < l_get_length(ts->tests); ++i) {
    test = (unit_test) l_get_item(ts->tests, i);
    result = (*test)();
    l_replace_item(ts->results, i, (void *) result);
    if (result == 0) {
      ts->passed += 1;
    } else {
      ts->failed += 1;
    }
  }
}
