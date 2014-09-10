// mirko.rahn@web.de

#ifndef CATCHUP_POINT_HPP
#define CATCHUP_POINT_HPP

#include <stream_modifier.hpp>

#include <tuple>
#include <vector>

namespace
{
  namespace point
  {
    typedef std::tuple<int, int, int> point;

    int x (point const&);
    int y (point const&);
    int z (point const&);
    std::vector<point> plane (int size);
    point rotate60 (point const&);
    point mirror (point const&, int axis);
  }
}

#endif
