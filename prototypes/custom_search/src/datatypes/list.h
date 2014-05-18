#ifndef LIST_H
#define LIST_H

// list.h
// Simple array-based lists.

#include <stdlib.h>
#include <stdint.h>

/***********
 * Globals *
 ***********/

// Defines what size memory chunk lists use internally.
extern size_t const LIST_CHUNK_SIZE;

// Defines how many empty chunks we should keep at the end of a shrinking list
// before reallocating to a smaller size.
extern size_t const LIST_KEEP_CHUNKS;

/**************
 * Structures *
 **************/

// An array-based list:
struct list_s;
typedef struct list_s list;

// Note: list_s is defined in list.c, not here. This is intentional: external
// code should only use list pointers and shouldn't deal with the internals of
// lists directly.

/*************
 * Functions *
 *************/

// Allocates and sets up a new empty list:
list *create_list(void);

// Frees the memory associated with a list.
void cleanup_list(list *l);

// Frees the memory associated with a list, and also calls free on each element
// in the list.
void destroy_list(list *l);


// Tests whether the given list is empty.
inline int l_is_empty(list *l);

// Returns the length of the given list.
inline size_t l_get_length(list *l);

// Test whether the given list contains the given element (uses address
// comparison).
int l_contains(list *l, void *element);

// Returns the ith element of the given list, or NULL if i is out of range.
void * l_get_item(list *l, size_t i);

// Returns a pointer to the ith element of the given list. This function should
// normally be avoided and the pointers it returns aren't safe to use if the
// list grows or shrinks. It also doesn't do any bounds checking. This is why
// it has an extra underscore at the beginning of its name.
void ** _l_get_pointer(list *l, size_t i);

// Replaces the element at index i with the given element, returning the
// replaced value. If i is out-of-range, it does nothing and returns NULL.
void * l_remove_item(list *l, size_t i);

// Removes the given number of items starting at the given index. The delete
// version frees the items before removing them. If any part of the range is
// out-of-bounds, it does nothing.
void l_remove_range(list *l, size_t i, size_t n);
void l_delete_range(list *l, size_t i, size_t n);

// Replaces the element at index i with the given element, returning the
// replaced value. If i is out-of-range, it does nothing and returns NULL.
void * l_replace_item(list *l, size_t i, void *element);

// Adds the given element to the end of the given list. Allocates new memory to
// expand the list as necessary.
void l_append_element(list *l, void *element);

// Removes and returns the last element of the given list. Returns NULL if the
// list is already empty.
void * l_pop_element(list *l);

// Removes just the first copy of the given element from the given list (uses
// address comparison). Returns the removed element, or NULL if the given
// element wasn't found.
void * l_remove_element(list *l, void *element);

// Removes all copies of the given element from the given list (uses address
// comparison). Returns the number of elements removed.
int l_remove_all_elements(list *l, void *element);

// Reverses the given list. Doesn't allocate or free any memory.
void l_reverse(list *l);

// Runs the given function sequentially on each element in the list.
void l_foreach(list *l, void (*f)(void *));

// Runs the given function sequentially on each element in the list with the
// given extra argument as its second argument.
void l_witheach(list *l, void *arg, void (*f)(void *, void *));

// Scans the list until the given function returns non-zero, and returns the
// element that matched. Returns NULL if no match was found.
void * l_find_element(list *l, int (*match)(void *));

// Scans the list until the given function returns non-zero given the list
// element as its first argument and the reference as its second argument.
// Returns the element that matched. Returns NULL if no match was found.
void * l_scan_elements(list *l, void *ref, int (*match)(void *, void *));

// Counts the number of bytes of data/overhead used by the given list.
size_t l_data_size(list *l);
size_t l_overhead_size(list *l);

#endif //ifndef LIST_H
