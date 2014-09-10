// mirko.rahn@web.de

#include <numbered.hpp>

namespace
{
  std::map<point::point, int> numbered (std::vector<point::point> const& ps)
  {
    std::map<point::point, int> m;
    int k (0);

    for (point::point const& p : ps)
    {
      m.emplace (p, k++);
    }

    return m;
  }
}
