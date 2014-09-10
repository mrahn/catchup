// mirko.rahn@web.de

#include <point.hpp>

namespace
{
  namespace point
  {
    int x (point const& p) { return std::get<0> (p); }

    std::vector<point> plane (int size)
    {
      std::vector<point> p;

      for (int x (1 - size); x < size; ++x)
      {
        for (int y (1 - size); y < size; ++y)
        {
          for (int z (1 - size); z < size; ++z)
          {
            if (x + y + z == 0)
            {
              p.emplace_back (x, y, z);
            }
          }
        }
      }

      return p;
    }
  }
}
