#ifndef STORY_H
#define STORY_H

// story.h
// Defines the basic data structures that represent a story.

#include "act.h"
#include "prop.h"
#include "entity.h"

/**************
 * Structures *
 **************/

// An event is composed of an action and (optionally) a reaction.
struct event_s;
typedef struct event_s event;

/*********
 * Types *
 *********/

// TODO: Any of these?

/*************************
 * Structure Definitions *
 *************************/

struct event_s {
  act action;
  act reaction;
};

#endif // ifndef STORY_H
