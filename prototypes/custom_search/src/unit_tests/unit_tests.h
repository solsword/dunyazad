#ifndef UNIT_TESTS_H
#define UNIT_TESTS_H

// unit_tests.h
// Gathers all unit tests in one place.

#include "datatypes/list.h"

/***********
 * Globals *
 ***********/

// The global list of test suites to run:
extern list * ALL_TEST_SUITES;

/*************
 * Functions *
 *************/

// Sets up the unit test system.
void setup_unit_tests(void);

// Cleans up the unit test system.
void cleanup_unit_tests(void);

// Runs all of the unit tests.
void run_all_tests(void);

// Prints all of the test results (doesn't run the tests).
void print_all_results(void);

// Returns 1 if all tests passed and 0 otherwise.
int all_tests_passed(void);

// The main function.
int main(int argc, char **argv);

#endif //ifndef UNIT_TESTS_H
