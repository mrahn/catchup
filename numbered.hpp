// mirko.rahn@web.de

#ifndef CATCHUP_NUMBERED_HPP
#define CATCHUP_NUMBERED_HPP

#include <point.hpp>

#include <map>
#include <vector>

namespace
{
  std::map<point::point, int> numbered (std::vector<point::point> const&);
}

#endif
