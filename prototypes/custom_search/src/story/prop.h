#ifndef PROP_H
#define PROP_H

// prop.h
// Defines the basic data structures that represent a property.

#include <stdint.h>

/**************
 * Structures *
 **************/

// A status is a simple binary property of a character (e.g., sick).
struct status_s;
typedef struct status_s status;

// A state is a named, discrete-valued property of a character (e.g., wealth).
struct state_s;
typedef struct state_s state;

// A link is a named, discrete-valued link between two characters
// (e.g., indebted_to):
struct link_s;
typedef struct link_s link;

// A relationship is a named, discrete-valued link between two characters
// (e.g., family_relationship):
struct rel_s;
typedef struct rel_s rel;

// All of the above are types of property:
struct property_s;
typedef struct property_s property;

/*********
 * Types *
 *********/

enum property_type_e {
  PRTYPE_STATUS,
  PRTYPE_STATE,
  PRTYPE_LINK,
  PRTYPE_REL
};
typedef property_type_e property_type;

/*************************
 * Structure Definitions *
 *************************/

struct status_s {
};

struct state_s {
};

struct link_s {
};

struct rel_s {
};

struct property_s {
  property_type which;
  void * contents;
};

#endif // ifndef PROP_H
