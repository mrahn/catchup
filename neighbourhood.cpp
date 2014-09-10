// mirko.rahn@web.de

#include <neighbourhood.hpp>

#include <point.hpp>

namespace
{
  template<int SIZE>
  neighbourhood<SIZE>::neighbourhood()
    : _neighbours()
  {
    std::vector<point::point> const points (point::plane (SIZE));

    int k (num_fields (SIZE) + 1);
    int f (0);

    for (point::point const& p : points)
    {
      _neighbours[f++] = k;

      for (int q (0); q < num_fields (SIZE); ++q)
      {
        if (point::distance (p, points[q]) == 1)
        {
          _neighbours[k++] = q;
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
