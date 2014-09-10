// mirko.rahn@web.de

#include <neighbourhood.hpp>

#include <numbered.hpp>
#include <point.hpp>

namespace
{
  template<int SIZE>
  neighbourhood<SIZE>::neighbourhood()
    : _neighbours()
  {
    std::vector<point::point> const points (point::plane (SIZE));
    std::map<point::point, int> const id_by_point (numbered (points));

    int k (num_fields (SIZE) + 1);
    int f (0);

    for (point::point const& p : points)
    {
      _neighbours[f++] = k;

      for (point::point const& q : points)
      {
        if (point::distance (p, q) == 1)
        {
          _neighbours[k++] = id_by_point.at (q);
        }
      }
    }

    _neighbours[f] = k;
  }
  template<int SIZE>
  int const* neighbourhood<SIZE>::neighbours() const
  {
    return _neighbours;
  }
}
