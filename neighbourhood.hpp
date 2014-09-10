// mirko.rahn@web.de

#ifndef CATCHUP_NEIGHBOURHOOD_HPP
#define CATCHUP_NEIGHBOURHOOD_HPP

#include <constant.hpp>

namespace
{
  template<int SIZE>
  class neighbourhood
  {
  public:
    neighbourhood();
    int const* neighbours() const;
  private:
    int _neighbours[num_fields (SIZE) + 1 + num_neighbours (SIZE)];
  };
}

#endif
