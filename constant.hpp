// mirko.rahn@web.de

#ifndef CATCHUP_CONSTANT_HPP
#define CATCHUP_CONSTANT_HPP

namespace
{
  constexpr int num_fields (int size)
  {
    return 3 * size * (size - 1) + (size < 1 ? size : 1);
  }
  constexpr int num_neighbours (int size)
  {
    return (size < 1) ? 0 : 6 * (size - 1) * (3 * size - 2);
  }
}

#endif
