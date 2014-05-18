// queue.c
// Simple dynamically-sized array-based queues.

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>

#include "queue.h"

/****************
 * Local Macros *
 ****************/

// An index wrapped within the domain of a queue:
#define QWRAP(q,idx) ((idx) % (q->size * QUEUE_CHUNK_SIZE))

// An translation from a queue index into an elements array index:
#define QIDX(q,idx) ((q->tail + idx) % (q->size * QUEUE_CHUNK_SIZE))

/***********
 * Globals *
 ***********/

// We're more worried about mallocs than about saving space:
size_t const QUEUE_CHUNK_SIZE = 256;
size_t const QUEUE_EXPAND_RATIO = 2;
size_t const QUEUE_SHRINK_THRESHOLD = 8;
size_t const QUEUE_SHRINK_RATIO = 4;
size_t const QUEUE_MIN_CHUNKS = 2;

/*************************
 * Structure Definitions *
 *************************/

struct queue_s {
  size_t size; // Measured in chunks, not entries.
  size_t count; // Measured in entries.
  size_t tail; // The index of the oldest item in the queue.
  void **elements;
};

/*********************
 * Private Functions *
 *********************/

static inline void grow_if_necessary(queue *q) {
  size_t i;
  if (q->count == q->size*QUEUE_CHUNK_SIZE) { // We need more memory.
    void ** new_elements = (void **) realloc(
      q->elements,
      sizeof(void *) * ((q->size * QUEUE_EXPAND_RATIO)*QUEUE_CHUNK_SIZE)
    );
    if (new_elements == NULL) {
      perror("Failed to allocate additional queue chunk.");
      exit(errno);
    }
    q->elements = new_elements;
    // If our queue was wrapped in the smaller space, unwrap it to be
    // contiguous in the new larger space:
    if ((q->tail + q->count) > (q->size * QUEUE_CHUNK_SIZE)) {
      for (i = 0; i < QIDX(q, q->count); ++i) {
        // Note no wrapping of the index is necessary here because we doubled
        // the size of the memory block:
        q->elements[(q->size * QUEUE_CHUNK_SIZE) + i] = q->elements[i];
      }
    }
    // Only update our size after using the old size above to unwrap stuff:
    q->size *= QUEUE_EXPAND_RATIO;
  }
}

static inline void shrink_if_necessary(queue *q) {
  size_t i;
  if (
    (q->size/QUEUE_SHRINK_RATIO) > QUEUE_MIN_CHUNKS
  &&
    q->count < (q->size/QUEUE_SHRINK_THRESHOLD)*QUEUE_CHUNK_SIZE
  ) {
    // We should free our extra elements, but we need to realign them at the
    // beginning of the array first.
    // First, if our queue is wrapped from the end of the array to the
    // beginning, start by copying the part starting from the beginning to its
    // proper destination:
    if ((q->tail + q->count) > (q->size * QUEUE_CHUNK_SIZE)) {
      for (i = 0; i < QIDX(q, q->count); ++i) {
        // Note no wrapping of the index is necessary here because we know that
        // the new queue count will fit in the shrunken memory block. We also
        // don't need to worry about overwriting the tail of the queue because
        // of our guarantee that the total count is small in relation to the
        // current queue size.
        q->elements[
          ((q->size * QUEUE_CHUNK_SIZE) - q->tail) + i
        ] = q->elements[i];
      }
    }
    // Whether the queue was wrapped or not, copy elements starting from the
    // tail to the front of the array, stopping at either the end of the array
    // or the end of the elements, whichever comes first:
    for (
      i = 0;
      i < q->count && i < (q->size * QUEUE_CHUNK_SIZE) - q->tail;
      ++i
    ) {
      q->elements[i] = q->elements[q->tail + i];
    }
    void ** new_elements = (void **) realloc(
      q->elements,
      sizeof(void *) * ((q->size/QUEUE_SHRINK_RATIO)*QUEUE_CHUNK_SIZE)
    );
    if (new_elements == NULL) {
      perror("Failed to remove empty queue space.");
      exit(errno);
    }
    q->elements = new_elements;
    q->size /= QUEUE_SHRINK_RATIO;
    q->tail = 0;
  }
}

/*************
 * Functions *
 *************/

queue *create_queue(void) {
  queue *q = (queue *) malloc(sizeof(queue));
  if (q == NULL) {
    perror("Failed to create queue.");
    exit(errno);
  }
  q->elements = (void **) malloc(sizeof(void *)*QUEUE_CHUNK_SIZE);
  if (q->elements == NULL) {
    perror("Failed to create initial queue chunk.");
    exit(errno);
  }
  q->size = 1;
  q->count = 0;
  q->tail = 0;
  return q;
}

void cleanup_queue(queue *q) {
  q->count = 0;
  q->size = 0;
  free(q->elements);
  free(q);
}

void destroy_queue(queue *q) {
  size_t i;
  for (i = 0; i < q->count; ++i) {
    free(q->elements[QIDX(q, i)]);
  }
  q->count = 0;
  q->size = 0;
  free(q->elements);
  free(q);
}


inline int q_is_empty(queue *q) {
  return (q->count == 0);
}

inline size_t q_get_length(queue *q) {
  return q->count;
}

int q_contains(queue *q, void *element) {
  size_t i;
  int result = 0;
  for (i = 0; i < q->count; ++i) {
    if (q->elements[QIDX(q, i)] == element) {
      result = 1;
      break;
    }
  }
  return result;
}

void * q_get_item(queue *q, size_t i) {
  if (i >= q->count) {
    return NULL;
  }
  return q->elements[QIDX(q, i)];
}

void q_push_element(queue *q, void *element) {
  grow_if_necessary(q);
  q->elements[QIDX(q, q->count)] = element;
  q->count += 1;
}

void * q_pop_element(queue *q) {
  void *result = NULL;
  if (q->count == 0) {
#ifdef DEBUG
    fprintf(stderr, "Warning: Pop from empty queue.\n");
#endif
    return NULL;
  }
  result = q->elements[q->tail];
  q->count -= 1;
  q->tail = QIDX(q, 1);
  shrink_if_necessary(q);
  return result;
}

void* q_remove_element(queue *q, void *element) {
  size_t i, j;
  void *result = NULL;
  for (i = 0; i < q->count; ++i) {
    if (q->elements[QIDX(q, i)] == element) {
      result = q->elements[QIDX(q, i)];
      for (j = i; j < (q->count - 1); ++j) {
        q->elements[QIDX(q, j)] = q->elements[QIDX(q, j + 1)];
      }
      q->count -= 1;
      shrink_if_necessary(q);
      break;
    }
  }
  return result;
}

int q_remove_all_elements(queue *q, void *element) {
  size_t i;
  size_t removed = 0;
  size_t skip = 0;
  for (i = 0; i + skip < q->count; ++i) {
    while (i + skip < q->count && q->elements[QIDX(q, i + skip)] == element) {
      skip += 1;
      removed += 1;
    }
    if (skip > 0 && i + skip < q->count) {
      q->elements[QIDX(q, i)] = q->elements[QIDX(q, i + skip)];
    }
  }
  q->count -= removed;
  shrink_if_necessary(q);
  return removed;
}

void q_foreach(queue *q, void (*f)(void *)) {
  size_t i;
  for (i = 0; i < q->count; ++i) {
    (*f)(q->elements[QIDX(q, i)]);
  }
}

void q_witheach(queue *q, void *arg, void (*f)(void *, void *)) {
  size_t i;
  for (i = 0; i < q->count; ++i) {
    (*f)(q->elements[QIDX(q, i)], arg);
  }
}

void * q_find_element(queue *q, int (*match)(void *)) {
  size_t i;
  for (i = 0; i < q->count; ++i) {
    if ((*match)(q->elements[QIDX(q, i)])) {
      return q->elements[QIDX(q, i)];
    }
  }
  return NULL;
}

void * q_scan_elements(queue *q, void *ref, int (*match)(void *, void *)) {
  size_t i;
  for (i = 0; i < q->count; ++i) {
    if ((*match)(q->elements[QIDX(q, i)], ref)) {
      return q->elements[QIDX(q, i)];
    }
  }
  return NULL;
}

size_t q_data_size(queue *q) {
  return q->count * sizeof(void *);
}

size_t q_overhead_size(queue *q) {
  return sizeof(q) + (q->size * QUEUE_CHUNK_SIZE - q->count) * sizeof(void *);
}

/******************************
 * Undefines for Local Macros *
 ******************************/

#undef QWRAP
#undef QIDX
