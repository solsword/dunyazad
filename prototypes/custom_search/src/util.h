#ifndef UTIL_H
#define UTIL_H

// util.h
// Various small utilities.

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/**********
 * MACROS *
 **********/

// Bits in a type:
#define bits(t) (sizeof(t) * 8ULL)

// All 1s except the sign bit:
#define lull(t) ((0x1ULL << (bits(t) - 1ULL)) - 1ULL)

// Various fully-shifted bytes:
#define bull(t) (0x8ULL << (bits(t) - 4ULL))
#define full(t) (0xFULL << (bits(t) - 4ULL))
#define tull(t) (0x7ULL << (bits(t) - 4ULL))

// Max for unsigned is all 1s:
#define umaxof(t) ( (t) (full(t) | lull(t)) )

// Min for signed is just the sign bit:
#define sminof(t) ( (t) bull(t) )
// Max for signed is all 1s except the sign bit:
#define smaxof(t) ( (t) (tull(t) | lull(t)) )

/*************
 * Constants *
 *************/

// radians <-> degrees
static float const R2D = 180*M_1_PI;
static float const D2R = M_PI/180;

/********************
 * Inline Functions *
 ********************/

// Faster floor function (mostly 'cause we're ignoring IEEE error stuff):
static inline int fastfloor(float x) {
  int ix = (int) x;
  return ix - (ix > x);
}
// Likewise for ceil():
static inline int fastceil(float x) {
  int ix = (int) x;
  return ix + (ix < x);
}

// Normalizes the given angle to be between -M_PI and M_PI.
static inline void norm_angle(float *angle) {
    while (*angle > M_PI) {
      *angle -= M_PI*2;
    }
    while (*angle < -M_PI) {
      *angle += M_PI*2;
    }
}

// Reads a file and returns a malloc'd char *:
static inline char * load_file(char const * const filename, size_t *size) {
  char * buffer = NULL;
  FILE * f = fopen(filename, "rb");

  if (f == NULL) {
    fprintf(
      stderr,
      "Error: unable to open file '%s'.\n",
      filename
    );
    exit(1);
  }
  fseek (f, 0, SEEK_END);
  *size = ftell(f);
  fseek (f, 0, SEEK_SET);
  buffer = malloc(*size);
  if (buffer == NULL) {
    fprintf(
      stderr,
      "Error: unable to allocate memory to read file '%s'.\n",
      filename
    );
    fclose(f);
    exit(1);
  }
  fread(buffer, 1, *size, f);
  fclose(f);
  return buffer;
}

#endif // ifndef UTIL_H
