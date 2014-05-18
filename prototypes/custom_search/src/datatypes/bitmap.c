// bitmap.c
// Bitmaps for recording booleans.

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>

#include "bitmap.h"

/*********
 * Types *
 *********/

// A single row of a bitmap (only used internally)
typedef uint64_t bitmap_row;

#define BITMAP_ROW_WIDTH 64

/*************************
 * Structure Definitions *
 *************************/

struct bitmap_s {
  size_t size; // how many entries are in this bitmap
  size_t rows; // how many rows there are
  bitmap_row *data; // the bits
};

/*********************
 * Private Functions *
 *********************/

// Returns the specified bit (either 1 or 0). Returns 0 for out-of-range
// indices.
static inline size_t bm_get_bit(bitmap *bm, size_t index) {
#ifdef DEBUG_CHECK_BITMAP_ACCESS
  if (index >= bm->size) {
    fprintf(stderr, "Error: out-of-range bitmap access in bm_get_bit.\n");
    exit(1);
  }
#endif
  return (bm->data[index / BITMAP_ROW_WIDTH] >> (index % BITMAP_ROW_WIDTH)) & 1;
}

// Sets the specified bit to 1.
static inline void bm_set_bit(bitmap *bm, size_t index) {
#ifdef DEBUG_CHECK_BITMAP_ACCESS
  if (index >= bm->size) {
    fprintf(stderr, "Error: out-of-range bitmap access in bm_set_bit.\n");
    exit(1);
  }
#endif
  bm->data[index / BITMAP_ROW_WIDTH] |= 1 << (index % BITMAP_ROW_WIDTH);
}

// Sets the specified bit to 0.
static inline void bm_clear_bit(bitmap *bm, size_t index) {
#ifdef DEBUG_CHECK_BITMAP_ACCESS
  if (index >= bm->size) {
    fprintf(stderr, "Error: out-of-range bitmap access in bm_clear_bit.\n");
    exit(1);
  }
#endif
  bm->data[index / BITMAP_ROW_WIDTH] &= ~(1 << (index % BITMAP_ROW_WIDTH));
}

/******************************
 * Constructors & Destructors *
 ******************************/

bitmap *create_bitmap(size_t bits) {
  bitmap *bm = (bitmap *) malloc(sizeof(bitmap));
  bm->size = bits;
  bm->rows = (bits / BITMAP_ROW_WIDTH) + (bits % BITMAP_ROW_WIDTH > 0);
  bm->data = (bitmap_row *) malloc(sizeof(bitmap_row) * bm->rows);
  return bm;
}

void cleanup_bitmap(bitmap *bm) {
  free(bm->data);
  free(bm);
}

/*************
 * Functions *
 *************/

ptrdiff_t bm_find_space(bitmap *bm, size_t required) {
  size_t i, j;
  int hit = 0;
  for (i = 0; i <= bm->size - required;) {
    hit = 1;
    for (j = i; j < i + required; ++j) {
      if (bm_get_bit(bm, j)) {
        i = j + 1;
        hit = 0;
        break;
      }
    }
    if (hit) {
      return i;
    }
  }
  return -1;
}

void bm_set_bits(bitmap *bm, size_t index, size_t size) {
  size_t i;
  for (i = index; i < index + size && i < bm->size; ++i) {
    bm_set_bit(bm, i);
  }
}
void bm_clear_bits(bitmap *bm, size_t index, size_t size) {
  size_t i;
  for (i = index; i < index + size && i < bm->size; ++i) {
    bm_clear_bit(bm, i);
  }
}
