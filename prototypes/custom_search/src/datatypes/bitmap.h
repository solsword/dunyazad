#ifndef BITMAP_H
#define BITMAP_H

// bitmap.h
// Bitmaps for recording booleans.

#include <stddef.h>

/**************
 * Structures *
 **************/

// A bitmap holding boolean info for a bunch of things.
struct bitmap_s;
typedef struct bitmap_s bitmap;

/******************************
 * Constructors & Destructors *
 ******************************/

// Allocates and returns a new bitmap.
bitmap *create_bitmap(size_t bits);

// Frees the data allocated for the given bitmap.
void cleanup_bitmap(bitmap *bm);

/*************
 * Functions *
 *************/

// Finds an open block of the required size in the bitmap, and returns its
// index. Returns -1 if there is no open block of the requested size.
ptrdiff_t bm_find_space(bitmap *bm, size_t required);

// Set/clear size bits starting at index.
void bm_set_bits(bitmap *bm, size_t index, size_t size);
void bm_clear_bits(bitmap *bm, size_t index, size_t size);

#endif //ifndef BITMAP_H
