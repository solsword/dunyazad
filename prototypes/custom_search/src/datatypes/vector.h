#ifndef VECTOR_H
#define VECTOR_H

// vector.h
// 3D floating point vectors.

#include <math.h>

#include "util.h"

/**************
 * Structures *
 **************/

// A vector of 3 floats.
struct vector_s;
typedef struct vector_s vector;

/*************
 * Constants *
 *************/

extern vector const V_UP;
extern vector const V_DOWN;
extern vector const V_NORTH;
extern vector const V_SOUTH;
extern vector const V_EAST;
extern vector const V_WEST;

/*************************
 * Structure Definitions *
 *************************/

struct vector_s {
  float x, y, z;
};

/********************
 * Inline Functions *
 ********************/

// Returns the magnitude of the given vector.
static inline float vmag(vector const * const v) {
  return sqrtf(v->x*v->x + v->y*v->y + v->z*v->z);
}

// Returns the squared magnitude of the given vector.
static inline float vmag2(vector const * const v) {
  return v->x*v->x + v->y*v->y + v->z*v->z;
}

// Returns the dot product of first and second.
static inline float vdot(
  vector const * const first,
  vector const * const second
) {
  return (
    (first->x * second->x) +
    (first->y * second->y) +
    (first->z * second->z)
  );
}

// Returns the scalar projection of vec onto onto.
static inline float vprojection(
  vector const * const vec,
  vector const * const onto
) {
  return vdot(vec, onto) / vmag(onto);
}

// Returns the scalar projection of vec onto onto, expressed in terms of the
// magnitude of onto.
static inline float vnormproj(
  vector const * const vec,
  vector const * const onto
) {
  return vdot(vec, onto) / vmag2(onto);
}

// Scales the given vector by the given scalar.
static inline void vscale(vector *v, float scale) {
  v->x *= scale;
  v->y *= scale;
  v->z *= scale;
}

// Rotates the vector around the z-axis by the given angle theta.
static inline void vyaw(vector *v, float yaw) {
  float ox = v->x;
  v->x = (v->x)*cosf(yaw) - (v->y)*sinf(yaw);
  v->y = (ox)*sinf(yaw) + (v->y)*cosf(yaw);
}

// Rotates the vector around its perpendicular in the x-y plane by the given
// angle phi.
static inline void vpitch(vector *v, float pitch) {
  float r = vmag(v);
  float rxy = sqrtf(v->x*v->x + v->y*v->y);
  float theta = atan2(v->y, v->x);
  float phi = atan2(v->z, rxy);
  v->x = (r * cosf(theta)) * cosf(phi);
  v->y = (r * sinf(theta)) * cosf(phi);
  v->z = r * sinf(phi);
}

// Overwrites the given vector with all zeroes.
static inline void vzero(vector *v) {
  v->x = 0;
  v->y = 0;
  v->z = 0;
}

// Overwrites the given vector with a unit vector facing in the given
// direction. North is yaw=0, pitch=0 is horizontal.
static inline void vface(vector *v, float yaw, float pitch) {
  v->x = -sinf(yaw) * cosf(pitch);
  v->y = cosf(yaw) * cosf(pitch);
  v->z = sinf(pitch);
}

// Normalizes the given vector so that its magnitude is 1 (or as close as
// possible given floating point math).
static inline void vnorm(vector *v) {
  float m = vmag(v);
  m += (m == 0);
  vscale(v, 1.0/m);
}

// Adds the given value vector to the target vector, storing the result in the
// target vector.
static inline void vadd(vector * const target, vector const * const value) {
  target->x += value->x;
  target->y += value->y;
  target->z += value->z;
}

// Adds the given value vector scaled by the given scalar to the target vector,
// storing the result in the target vector.
static inline void vadd_scaled(
  vector * const target,
  vector const * const value,
  float scale
) {
  target->x += value->x * scale;
  target->y += value->y * scale;
  target->z += value->z * scale;
}

// Subtracts the given value vector from the target vector, storing the result
// in the target vector.
static inline void vsub(vector * const target, vector const * const value) {
  target->x -= value->x;
  target->y -= value->y;
  target->z -= value->z;
}

// Copies the given value vector into the given target vector.
static inline void vcopy(vector * const target, vector const * const value) {
  target->x = value->x;
  target->y = value->y;
  target->z = value->z;
}

// Projects the given vector onto the given 'onto' vector, storing the result
// in the given vector.
static inline void vproject(
  vector * const vec,
  vector const * const onto
) {
  float projscale = vnormproj(vec, onto);
  vcopy(vec, onto);
  vscale(vec, projscale);
}

// Computes the cross product of the given vectors A and B and stores the
// result in the given result vector.
static inline void vcross(
  vector * const result,
  vector const * const A,
  vector const * const B
) {
  result->x =   (A->y * B->z) - (B->y * A->z);
  result->y = -((A->x * B->z) - (B->x * A->z));
  result->z =   (A->x * B->y) - (B->x * A->y);
}

// Takes an input vector 'vec' and three basis vectors i, j, and k and rewrites
// the given input vector so that its components are expressed in terms of the
// given basis vectors. After this operation, (i*vec.x + j*vec.y + k*vec.z) is
// equal to the old vector.
static inline void vintermsof(
  vector * const vec,
  vector const * const i,
  vector const * const j,
  vector const * const k
) {
  float tx, ty, tz;
  tx = vnormproj(vec, i);
  ty = vnormproj(vec, j);
  tz = vnormproj(vec, k);
  vec->x = tx;
  vec->y = ty;
  vec->z = tz;
}

// Performs linear interpolation between vectors a and b according to parameter
// t (0 <= t <= 1 should hold, otherwise it's extrapolation). Puts the result
// into the given result vector.
static inline void lerp(vector *a, vector *b, float t, vector *result) {
  float ti = 1 - t;
  result->x = a->x * ti + b->x * t;
  result->y = a->y * ti + b->y * t;
  result->z = a->z * ti + b->z * t;
}

// Rotates the given vector by the given amount around the positive z axis.
static inline void vrotz(vector *v, float radians) {
  float tx;
  tx = v->x * cosf(radians) - v->y * sinf(radians);
  v->y = v->x * sinf(radians) + v->y * cosf(radians);
  v->x = tx;
}

/*************
 * Functions *
 *************/

#endif //ifndef VECTOR_H
