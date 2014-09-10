// mirko.rahn@web.de

#include <point.hpp>

namespace
{
  namespace point
  {
    int x (point const& p) { return std::get<0> (p); }
    int y (point const& p) { return std::get<1> (p); }
    int z (point const& p) { return std::get<2> (p); }

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

    point rotate60 (point const& p)
    {
      return point ( (2 * x (p) -     y (p) + 2 * z (p)) / 3
                   , (2 * x (p) + 2 * y (p) -     z (p)) / 3
                   , (   -x (p) + 2 * y (p) + 2 * z (p)) / 3
                   );
    }

    point mirror (point const& p, int k)
    {
      switch (k)
      {
      case 0: return point (x (p), y (p), z (p));
      case 1: return point (x (p), z (p), y (p));
      case 2: return point (y (p), x (p), z (p));
      case 3: return point (y (p), z (p), x (p));
      case 4: return point (z (p), x (p), y (p));
      case 5: return point (z (p), y (p), x (p));
      }

      abort();
    }
  }
}
