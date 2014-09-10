// mirko.rahn@web.de

#include <player.hpp>

namespace
{
  namespace player
  {
    player other (player p)
    {
      return (p == Blue) ? Orange : Blue;
    }

    show::show (player const& player)
      : _player (player)
    {}
    std::ostream& show::operator() (std::ostream& os) const
    {
      return os << ( (_player == Blue) ? 'B'
                   : (_player == Orange) ? 'O'
                   : '.'
                   );
    }
  }
}
