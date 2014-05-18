// vector.c
// 3D floating point vectors.

#include "vector.h"

/*************
 * Constants *
 *************/

vector const     V_UP = { .x=  0.0, .y=  0.0, .z=  1.0 };
vector const   V_DOWN = { .x=  0.0, .y=  0.0, .z= -1.0 };
vector const  V_NORTH = { .x=  0.0, .y=  1.0, .z=  0.0 };
vector const  V_SOUTH = { .x=  0.0, .y= -1.0, .z=  0.0 };
vector const   V_EAST = { .x=  1.0, .y=  0.0, .z=  0.0 };
vector const   V_WEST = { .x= -1.0, .y=  0.0, .z=  0.0 };

/*************
 * Functions *
 *************/
