#undef TEST_SUITE_NAME
#undef TEST_SUITE_TESTS
#define TEST_SUITE_NAME map3
#define TEST_SUITE_TESTS { \
    &test_map3_setup_cleanup, \
    &test_map3_growth, \
    &test_map3_put_pop, \
    &test_map3_put_pop_2, \
    &test_map3_collision, \
    NULL, \
  }

#ifndef TEST_MAP3_H
#define TEST_MAP3_H

#include <stdio.h>

#include "datatypes/map.h"


// Note: profiling these unit tests suggests that map3 is ~5-10x faster than
// the more flexible default map datatype for inserts but has similar
// performance for reads and pops.

size_t test_map3_setup_cleanup(void) {
  int i;
  map *m;
  for (i = 0; i < 1000; ++i) {
    m = create_map(3, 1024);
    cleanup_map(m);
  }
  return 0;
}

size_t test_map3_growth(void) {
  size_t i = 0;
  map *m = create_map(3, 1024);
  for (i = 0; i < 10000; ++i) {
    if (
      m3_put_value(
        m,
        NULL,
        (map_key_t) i,
        (map_key_t) i + 1,
        (map_key_t) i + 2
      ) != NULL
    ) { return 1; }
  }
  cleanup_map(m);
  return 0;
}

size_t test_map3_put_pop(void) {
  map *m = create_map(3, 1024);
  if (
    m3_put_value(
      m,
      (void *) 17,
      (map_key_t) 3,
      (map_key_t) 4,
      (map_key_t) 3
    ) != NULL
  ) { return 1; }
  if (
    m3_get_value(
      m,
      (map_key_t) 3,
      (map_key_t) 4,
      (map_key_t) 3
    ) != (void *) 17
  ) { return 2; }
  if (
    m3_pop_value(
      m,
      (map_key_t) 3,
      (map_key_t) 4,
      (map_key_t) 3
    ) != (void *) 17
  ) { return 3; }
  if (
    m3_pop_value(
      m,
      (map_key_t) 3,
      (map_key_t) 4,
      (map_key_t) 3
    ) != NULL
  ) { return 4; }
  if (
    m3_pop_value(
      m,
      (map_key_t) 3,
      (map_key_t) 4,
      (map_key_t) 3
    ) != NULL
  ) { return 5; }
  if (
    m3_pop_value(
      m,
      (map_key_t) 7,
      (map_key_t) 6,
      (map_key_t) 5
    ) != NULL
  ) { return 6; }
  if (
    m3_put_value(
      m,
      (void *) 8,
      (map_key_t) 1,
      (map_key_t) 2,
      (map_key_t) 3
    ) != NULL
  ) { return 7; }
  if (
    m3_put_value(
      m,
      (void *) 9,
      (map_key_t) 3,
      (map_key_t) 4,
      (map_key_t) 3
    ) != NULL
  ) { return 8; }
  if (
    m3_put_value(
      m,
      (void *) 10,
      (map_key_t) 5,
      (map_key_t) 5,
      (map_key_t) 5
    ) != NULL
  ) { return 9; }
  if (
    m3_put_value(
      m,
      (void *) 11,
      (map_key_t) 5,
      (map_key_t) 5,
      (map_key_t) 5
    ) != (void *) 10
  ) { return 10; }
  if (
    m3_get_value(
      m,
      (map_key_t) 1,
      (map_key_t) 2,
      (map_key_t) 3
    ) != (void *) 8
  ) { return 11; }
  if (
    m3_get_value(
      m,
      (map_key_t) 3,
      (map_key_t) 4,
      (map_key_t) 3
    ) != (void *) 9
  ) { return 12; }
  if (
    m3_get_value(
      m,
      (map_key_t) 5,
      (map_key_t) 5,
      (map_key_t) 5
    ) != (void *) 11
  ) { return 13; }
  if (
    m3_pop_value(
      m,
      (map_key_t) 3,
      (map_key_t) 4,
      (map_key_t) 3
    ) != (void *) 9
  ) { return 14; }
  if (
    m3_pop_value(
      m,
      (map_key_t) 1,
      (map_key_t) 2,
      (map_key_t) 3
    ) != (void *) 8
  ) { return 15; }
  if (
    m3_pop_value(
      m,
      (map_key_t) 5,
      (map_key_t) 5,
      (map_key_t) 5
    ) != (void *) 11
  ) { return 16; }
  if (
    m3_pop_value(
      m,
      (map_key_t) 1,
      (map_key_t) 2,
      (map_key_t) 3
    ) != NULL
  ) { return 17; }
  cleanup_map(m);
  return 0;
}

size_t test_map3_put_pop_2(void) {
  size_t x = 0, y = 0, z = 0;
  map *m = create_map(3, 1024);
  if (
    m3_put_value(
      m,
      (void *) 17,
      (map_key_t) x,
      (map_key_t) y,
      (map_key_t) z
    ) != NULL
  ) { return 1; }

  x = 1; y = 0; z = 0;
  if (
    m3_put_value(
      m,
      (void *) 18,
      (map_key_t) x,
      (map_key_t) y,
      (map_key_t) z
    ) != NULL
  ) { return 2; }

  x = 1; y = 1; z = 1;
  if (
    m3_put_value(
      m,
      (void *) 19,
      (map_key_t) x,
      (map_key_t) y,
      (map_key_t) z
    ) != NULL
  ) { return 3; }
  if (m_get_count(m) != 3) { return 4; }

  if (
    m3_get_value(
      m,
      (map_key_t) x,
      (map_key_t) y,
      (map_key_t) z
    ) != (void *) 19
  ) { return 5; }
  if (
    m3_pop_value(
      m,
      (map_key_t) x,
      (map_key_t) y,
      (map_key_t) z
    ) != (void *) 19
  ) { return 6; }
  if (
    m3_get_value(
      m,
      (map_key_t) x,
      (map_key_t) y,
      (map_key_t) z
    ) != NULL
  ) { return 7; }
  if (m_get_count(m) != 2) { return 8; }

  x = 1; y = 0; z = 0;
  if (
    m3_get_value(
      m,
      (map_key_t) x,
      (map_key_t) y,
      (map_key_t) z
    ) != (void *) 18
  ) { return 9; }
  if (
    m3_pop_value(
      m,
      (map_key_t) x,
      (map_key_t) y,
      (map_key_t) z
    ) != (void *) 18
  ) { return 10; }
  if (
    m3_get_value(
      m,
      (map_key_t) x,
      (map_key_t) y,
      (map_key_t) z
    ) != NULL
  ) { return 11; }
  if (m_get_count(m) != 1) { return 12; }

  x = 0; y = 0; z = 0;
  if (
    m3_get_value(
      m,
      (map_key_t) x,
      (map_key_t) y,
      (map_key_t) z
    ) != (void *) 17
  ) { return 13; }
  if (
    m3_pop_value(
      m,
      (map_key_t) x,
      (map_key_t) y,
      (map_key_t) z
    ) != (void *) 17
  ) { return 14; }
  if (
    m3_get_value(
      m,
      (map_key_t) x,
      (map_key_t) y,
      (map_key_t) z
    ) != NULL
  ) { return 15; }
  if (m_get_count(m) != 0) { return 16; }
  if (!m_is_empty(m)) { return 17; }
  cleanup_map(m);
  return 0;
}

size_t test_map3_collision(void) {
  static size_t const batch_size = 100;
  static size_t const small_batch_size = 73;
  size_t x = 0, y = 0, z = 0;
  map *m = create_map(3, 1024);
  for (x = 0; x < batch_size; ++x) {
    for (y = 0; y < batch_size; ++y) {
      for (z = 0; z < batch_size; ++z) {
        m3_put_value(
          m,
          (void *) 17,
          (map_key_t) x,
          (map_key_t) y,
          (map_key_t) z
        );
      }
    }
  }
  if (
    m3_get_value(m, (map_key_t) 0, (map_key_t) 0, (map_key_t) 0) != (void *) 17
  ) { return 5; }
  if (
    m3_get_value(m, (map_key_t) 0, (map_key_t) 0, (map_key_t) 1) != (void *) 17
  ) { return 10; }
  if (
    m3_get_value(m, (map_key_t) 1, (map_key_t) 1, (map_key_t) 1) != (void *) 17
  ) { return 15; }
  if (
    !m3_contains_key(
      m,
      (map_key_t) 1,
      (map_key_t) (batch_size - 1),
      (map_key_t) (batch_size - 1)
    )
  ) { return 20; }
  if (
    m3_get_value(
      m,
      (map_key_t) 1,
      (map_key_t) (batch_size - 1),
      (map_key_t) (batch_size - 1)
    ) != (void *) 17
  ) { return 25; }
  if (
    m3_contains_key(
      m,
      (map_key_t) 0,
      (map_key_t) 0,
      (map_key_t) batch_size
    )
  ) { return 30; }
  if (
    m3_get_value(
      m,
      (map_key_t) 0,
      (map_key_t) 0,
      (map_key_t) batch_size
    ) != NULL
  ) { return 35; }
  if ( !m3_contains_key(m, (map_key_t) 7, (map_key_t) 96, (map_key_t) 12)) {
    return 40;
  }
  if (
    m3_get_value(
      m,
      (map_key_t) 7,
      (map_key_t) 96,
      (map_key_t) 12
    ) != (void *) 17
  ) { return 45; }
  if (
    m3_get_value(
      m,
      (map_key_t) batch_size - 1,
      (map_key_t) batch_size - 1,
      (map_key_t) batch_size - 1
    ) != (void *) 17
  ) { return 50; }

  for (x = 0; x < small_batch_size; ++x) {
    for (y = 0; y < small_batch_size; ++y) {
      for (z = 0; z < small_batch_size; ++z) {
        if (
          m3_pop_value(
            m,
            (map_key_t) x,
            (map_key_t) y,
            (map_key_t) z
          ) != (void *) 17
        ) { return 55; }
      }
    }
  }
  for (x = 0; x < batch_size; ++x) {
    m3_put_value(
      m,
      (void *) 35,
      (map_key_t) (x % 7),
      (map_key_t) ((x+2) % 7),
      (map_key_t) ((x+4) % 7)
    );
  }
  if (
    m3_get_value(
      m,
      (map_key_t) (small_batch_size + 1),
      (map_key_t) 0,
      (map_key_t) 0
    ) != (void *) 17
  ) { return 60; }
  if (
    m3_get_value(
      m,
      (map_key_t) 0,
      (map_key_t) 2,
      (map_key_t) 4
    ) != (void *) 35
  ) { return 65; }
  if (
    m3_get_value(
      m,
      (map_key_t) 0,
      (map_key_t) 0,
      (map_key_t) 0
    ) != NULL
  ) { return 70; }
  cleanup_map(m);
  return 0;
}

#endif //ifndef TEST_MAP3_H
