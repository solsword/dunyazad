// list.c
// Simple array-based lists.

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>

#include "list.h"

/***********
 * Globals *
 ***********/

// We're more worried about mallocs than about saving space:
size_t const LIST_CHUNK_SIZE = 64;
size_t const LIST_KEEP_CHUNKS = 4;

/*************************
 * Structure Definitions *
 *************************/

struct list_s {
  size_t size; // Measured in chunks, not entries.
  size_t count; // Measured in entries.
  void **elements;
};

/*********************
 * Private Functions *
 *********************/

static inline void grow_if_necessary(list *l) {
  if (l->count == l->size*LIST_CHUNK_SIZE) { // We need more memory.
    void ** new_elements = (void **) realloc(
      l->elements,
      sizeof(void *) * ((l->size + 1)*LIST_CHUNK_SIZE)
    );
    if (new_elements == NULL) {
      perror("Failed to allocate additional list chunk.");
      exit(errno);
    }
    l->elements = new_elements;
    l->size += 1;
  }
}

static inline void shrink_if_necessary(list *l) {
  if (
    l->size > LIST_KEEP_CHUNKS
  &&
    l->count < (l->size - LIST_KEEP_CHUNKS)*LIST_CHUNK_SIZE
  ) {
    // We should free our extra elements.
    void ** new_elements = (void **) realloc(
      l->elements,
      sizeof(void *) * ((l->size - LIST_KEEP_CHUNKS)*LIST_CHUNK_SIZE)
    );
    if (new_elements == NULL) {
      perror("Failed to remove empty list chunks.");
      exit(errno);
    }
    l->elements = new_elements;
    l->size -= LIST_KEEP_CHUNKS;
  }
}

/*************
 * Functions *
 *************/

list *create_list(void) {
  list *l = (list *) malloc(sizeof(list));
  if (l == NULL) {
    perror("Failed to create list.");
    exit(errno);
  }
  l->elements = (void **) malloc(sizeof(void *) * LIST_CHUNK_SIZE);
  if (l->elements == NULL) {
    perror("Failed to create initial list chunk.");
    exit(errno);
  }
  l->size = 1;
  l->count = 0;
  return l;
}

void cleanup_list(list *l) {
  l->count = 0;
  l->size = 0;
  free(l->elements);
  free(l);
}

void destroy_list(list *l) {
  size_t i;
  for (i = 0; i < l->count; ++i) {
    free(l->elements[i]);
  }
  l->count = 0;
  l->size = 0;
  free(l->elements);
  free(l);
}


inline int l_is_empty(list *l) {
  return (l->count == 0);
}

inline size_t l_get_length(list *l) {
  return l->count;
}

int l_contains(list *l, void *element) {
  size_t i;
  int result = 0;
  for (i = 0; i < l->count; ++i) {
    if (l->elements[i] == element) {
      result = 1;
      break;
    }
  }
  return result;
}

void * l_get_item(list *l, size_t i) {
  if (i >= l->count) {
#ifdef DEBUG
    fprintf(stderr, "Warning: l_get_item on item beyond end of list.\n");
#endif
    return NULL;
  }
  return l->elements[i];
}

void ** _l_get_pointer(list *l, size_t i) {
  return &(l->elements[i]);
}

void * l_remove_item(list *l, size_t i) {
  if (i >= l->count) {
#ifdef DEBUG
    fprintf(stderr, "Warning: l_remove_item on item beyond end of list.\n");
#endif
    return NULL;
  }
  size_t j;
  void *result = l->elements[i];
  for (j = i; j < (l->count - 1); ++j) {
    l->elements[j] = l->elements[j+1];
  }
  l->count -= 1;
  shrink_if_necessary(l);
  return result;
}

void l_remove_range(list *l, size_t i, size_t n) {
  size_t j;
  if (i + n > l->count) {
#ifdef DEBUG
    fprintf(stderr, "Warning: l_remove_range extends beyond end of list.\n");
#endif
    return;
  }
  for (j = i; j < (l->count - n); ++j) {
    l->elements[j] = l->elements[j+n];
  }
  l->count -= n;
  shrink_if_necessary(l);
}

void l_delete_range(list *l, size_t i, size_t n) {
  size_t j;
  if (i + n > l->count) {
#ifdef DEBUG
    fprintf(stderr, "Warning: l_delete_range extends beyond end of list.\n");
#endif
    return;
  }
  for (j = i; j < (l->count - n); ++j) {
    if (j - i < n - 1) {
      free(l->elements[j]);
    }
    l->elements[j] = l->elements[j+n];
  }
  l->count -= n;
  shrink_if_necessary(l);
}

void * l_replace_item(list *l, size_t i, void *element) {
  if (i >= l->count) {
#ifdef DEBUG
    fprintf(stderr, "Warning: l_replace_item on item beyond end of list.\n");
#endif
    return NULL;
  }
  void *tmp = l->elements[i];
  l->elements[i] = element;
  return tmp;
}

void l_append_element(list *l, void *element) {
  grow_if_necessary(l);
  l->elements[l->count] = element;
  l->count += 1;
}

void * l_pop_element(list *l) {
  void *result = NULL;
  if (l->count == 0) {
#ifdef DEBUG
    fprintf(stderr, "Warning: Pop from empty list.\n");
#endif
    return NULL;
  }
  result = l->elements[l->count - 1];
  l->count -= 1;
  shrink_if_necessary(l);
  return result;
}

void* l_remove_element(list *l, void *element) {
  size_t i, j;
  void *result = NULL;
  for (i = 0; i < l->count; ++i) {
    if (l->elements[i] == element) {
      result = l->elements[i];
      for (j = i; j < (l->count - 1); ++j) {
        l->elements[j] = l->elements[j+1];
      }
      l->count -= 1;
      shrink_if_necessary(l);
      break;
    }
  }
  return result;
}

int l_remove_all_elements(list *l, void *element) {
  size_t i;
  size_t removed = 0;
  size_t skip = 0;
  for (i = 0; i + skip < l->count; ++i) {
    while (i + skip < l->count && l->elements[i + skip] == element) {
      skip += 1;
      removed += 1;
    }
    if (skip > 0 && i + skip < l->count) {
      l->elements[i] = l->elements[i + skip];
    }
  }
  l->count -= removed;
  shrink_if_necessary(l);
  return removed;
}

void l_reverse(list *l) {
  size_t i;
  void *phased;
  for (i = 0; i < (l->count / 2); ++i) {
    phased = l->elements[l->count - i - 1];
    l->elements[l->count - i - 1] = l->elements[i];
    l->elements[i] = phased;
  }
}

void l_foreach(list *l, void (*f)(void *)) {
  size_t i;
  for (i = 0; i < l->count; ++i) {
    (*f)(l->elements[i]);
  }
}

void l_witheach(list *l, void *arg, void (*f)(void *, void *)) {
  size_t i;
  for (i = 0; i < l->count; ++i) {
    (*f)(l->elements[i], arg);
  }
}

void * l_find_element(list *l, int (*match)(void *)) {
  size_t i;
  for (i = 0; i < l->count; ++i) {
    if ((*match)(l->elements[i])) {
      return l->elements[i];
    }
  }
  return NULL;
}

void * l_scan_elements(list *l, void *ref, int (*match)(void *, void *)) {
  size_t i;
  for (i = 0; i < l->count; ++i) {
    if ((*match)(l->elements[i], ref)) {
      return l->elements[i];
    }
  }
  return NULL;
}

size_t l_data_size(list *l) {
  return l->count * sizeof(void *);
}

size_t l_overhead_size(list *l) {
  return sizeof(list) + (l->size * LIST_CHUNK_SIZE - l->count) * sizeof(void *);
}
