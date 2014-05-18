#ifndef TEST_SUITE_H
#define TEST_SUITE_H

// test_suite.h
// A simple unit testing harness.

#include "datatypes/list.h"

/*********
 * Types *
 *********/

typedef size_t (*unit_test)(void);

/**************
 * Structures *
 **************/

// A list-based suite of tests to run:
struct test_suite_s;
typedef struct test_suite_s test_suite;

/*************************
 * Structure Definitions *
 *************************/

struct test_suite_s {
  char const * name;
  list *tests;
  list *results;
  int passed;
  int failed;
};

/*************
 * Functions *
 *************/

// Allocates and sets up a new empty test suite.
test_suite *create_test_suite(char const * const name);

// Frees the memory associated with a test suite.
void cleanup_test_suite(test_suite *ts);

// Adds the given test to the given test suite.
void ts_add_test(test_suite *ts, unit_test test);

// Tests whether the given test suite passed (doesn't run it).
int ts_passed(test_suite *ts);

// Prints a summary of the test suite results (doesn't run it).
void ts_print_short_results(test_suite *ts);

// Prints pass/fail results for each test (doesn't run them).
void ts_print_detailed_results(test_suite *ts);

// Runs the tests in the given test suite.
void ts_run_tests(test_suite *ts);

#endif //ifndef TEST_SUITE_H
