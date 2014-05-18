#ifndef ACT_H
#define ACT_H

// act.h
// Defines the basic data structures that represent an action.

#include <stdint.h>

/**************
 * Structures *
 **************/

// An act (up to two per event).
struct act_s;
typedef struct act_s act;

// A motive explains the reason for an action.
struct motive_s;
typedef struct motive_s motive;

/*********
 * Types *
 *********/

typedef uint8_t act_type;

enum motive_type_e {
  MT_CULTURE, // an action that's normal and expected
  MT_RECIPROCATION, // in response to a previous action
  MT_PASSION, // in response to an emotion
  MT_PERSONALITY, // because of one's personality
  MT_FATE // no motivation
};
typedef enum motive_type_e motive_type;

/*************************
 * Structure Definitions *
 *************************/

struct act_s {
  act_type type;
};

struct motive_s {
  motive_type type;
  act *context; // for MT_RECIPROCATION
  property *emotion; // for MT_PASSION
  property *nature; // for MT_PERSONALITY
};

#endif // ifndef ACT_H
