// mirko.rahn@web.de

#ifndef CATCHUP_NEIGHBOURHOOD_HPP
#define CATCHUP_NEIGHBOURHOOD_HPP

#include <constant.hpp>

namespace
{
  template<int SIZE>
  struct neighbourhood
  {
    neighbourhood();

    int neighbours[num_fields (SIZE) + 1 + num_neighbours (SIZE)];
  };
}

#endif
