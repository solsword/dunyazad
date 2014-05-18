#ifndef QUEUE_H
#define QUEUE_H

// queue.h
// Simple dynamically-sized array-based queues.

#include <stdlib.h>
#include <stdint.h>

/***********
 * Globals *
 ***********/

// Defines what size memory chunk queues use internally.
extern size_t const QUEUE_CHUNK_SIZE;

// Defines the how much we multiply the queue size by when expanding the queue
// (shouldn't be less than 2 or errors may occur).
extern size_t const QUEUE_EXPAND_RATIO;

// Defines the ratio between the queue elements and the queue's array size that
// must hold before a queue shrink will be triggered:
extern size_t const QUEUE_SHRINK_THRESHOLD;

// Defines how much the queue shrinks when a shrink happens (the ratio between
// the new queue size and the old size). Must not be larger than the
// QUEUE_SHRINK_THRESHOLD.
extern size_t const QUEUE_SHRINK_RATIO;

// Defines the minimum number of chunks below which a queue will not shrink (it
// may not start with this many chunks though).
extern size_t const QUEUE_MIN_CHUNKS;

/**************
 * Structures *
 **************/

// An array-based queue  
struct queue_s;
typedef struct queue_s queue;

// Note: queue_s is defined in queue.c, not here. This is intentional: external
// code should only use queue pointers and shouldn't deal with the internals of
// queues directly.

/*************
 * Functions *
 *************/

// Allocates and sets up a new empty queue:
queue *create_queue(void);

// Frees the memory associated with a queue.
void cleanup_queue(queue *q);

// Frees the memory associated with a queue, and also calls free on each
// element in the queue.
void destroy_queue(queue *q);


// Tests whether the given queue is empty.
inline int q_is_empty(queue *q);

// Returns the length of the given queue.
inline size_t q_get_length(queue *q);

// Test whether the given queue contains the given element (uses address
// comparison).
int q_contains(queue *q, void *element);

// Returns the ith element of the given queue, or NULL if i is out of range.
void * q_get_item(queue *q, size_t i);

// Adds the given element to the end of the given queue. Allocates new memory
// to expand the queue if necessary.
void q_push_element(queue *q, void *element);

// Removes and returns the last element of the given queue. Returns NULL if the
// queue is already empty.
void * q_pop_element(queue *q);

// Removes just the first copy of the given element from the given queue (uses
// address comparison). Returns the removed element, or NULL if the given
// element wasn't found.
void * q_remove_element(queue *q, void *element);

// Removes all copies of the given element from the given queue (uses address
// comparison). Returns the number of elements removed.
int q_remove_all_elements(queue *q, void *element);

// Runs the given function sequentially on each element in the queue.
void q_foreach(queue *q, void (*f)(void *));

// Runs the given function sequentially on each element in the list with the
// given extra argument as its second argument.
void q_witheach(queue *q, void *arg, void (*f)(void *, void *));

// Scans the queue until the given function returns non-zero, and returns the
// element that matched. Returns NULL if no match was found.
void * q_find_element(queue *q, int (*match)(void *));

// Scans the queue until the given function returns non-zero given the queue
// element as its first argument and the given reference as its second
// argument. Returns the element that matched. Returns NULL if no match was
// found.
void * q_scan_elements(queue *q, void *ref, int (*match)(void *, void *));

// Counts the number of bytes of data/overhead used by the given queue.
size_t q_data_size(queue *q);
size_t q_overhead_size(queue *q);

#endif //ifndef QUEUE_H
