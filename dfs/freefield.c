#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned long field_t;

#define SIZE 7UL

#define USED dat[0UL]
#define PUT(x) dat[x + 1UL]
#define NEXT(x) dat[x + SIZE + 2UL]
#define PREV(x) dat[x + SIZE * 2UL + 2UL]
#define FIRST dat[SIZE + 1UL] - 1UL

static field_t* ff_malloc()
{
  field_t* dat = (field_t*) malloc ((3U * SIZE + 3U) * sizeof (field_t));

  if (dat == NULL)
  {
    fprintf (stderr, "failed to allocate");

    exit (EXIT_FAILURE);
  }

  USED = 0;

  for (unsigned i = SIZE + 1; i < 3 * SIZE + 3; ++i)
  {
    dat[i] = 1;
  }

  return dat;
}

static void ff_put (field_t* dat, field_t f)
{
  assert (f < SIZE);

  NEXT (f - PREV (f)) += NEXT (f);
  PREV (f + NEXT (f)) += PREV (f);

  PUT (USED++) = f;
}

static void ff_unput (field_t* dat)
{
  assert (USED > 0);

  field_t const f = PUT (--USED);

  NEXT (f - PREV (f)) -= NEXT (f);
  PREV (f + NEXT (f)) -= PREV (f);
}

int main()
{
  field_t* dat = ff_malloc();

  ff_put (dat, 2);
  ff_put (dat, 4);
  ff_put (dat, 6);
  ff_put (dat, 5);
  ff_put (dat, 3);
  ff_put (dat, 1);
  ff_put (dat, 0);

  printf ("%li:", SIZE - USED);
  for (field_t f = FIRST; f < SIZE; f += NEXT (f))
  {
    printf (" %li", f);
  }
  printf ("\n");

  ff_unput (dat);
  ff_unput (dat);
  ff_unput (dat);
  ff_unput (dat);
  ff_unput (dat);
  ff_unput (dat);
  ff_unput (dat);

  printf ("%li:", SIZE - USED);
  for (field_t f = FIRST; f < SIZE; f += NEXT (f))
  {
    printf (" %li", f);
  }
  printf ("\n");

  for (int i = 0; i < 3 * SIZE + 3; ++i)
  {
    printf (" %li", dat[i]);
  }
  printf ("\n");

  free (dat);

  return 0;
}
