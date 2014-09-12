// mirko.rahn@web.de

#ifndef CATCHUP_STREAM_MODIFIER_HPP
#define CATCHUP_STREAM_MODIFIER_HPP

#include <iostream>

namespace
{
  template<typename T>
  struct stream_modifier
  {
    virtual ~stream_modifier() = default;
    virtual std::ostream& operator() (std::ostream&) const = 0;
  };
  template<typename T>
  std::ostream& operator<< ( std::ostream& stream
                           , stream_modifier<T> const& modify
                           )
  {
    return modify (stream);
  }
}

#endif
