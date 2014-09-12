// mirko.rahn@web.de

#ifndef CATCHUP_PLAYER_HPP
#define CATCHUP_PLAYER_HPP

#include <stream_modifier.hpp>

namespace
{
  namespace player
  {
    typedef enum {Blue = 0, Orange = 1, None} player;

    player other (player p)
    {
      return (p == Blue) ? Orange : Blue;
    }

    class show : public stream_modifier<player>
    {
    public:
      show (player const& player)
        : _player (player)
      {}
      virtual std::ostream& operator() (std::ostream& os) const override
      {
        return os << ( (_player == Blue) ? 'B'
                     : (_player == Orange) ? 'O'
                     : '.'
                     );
      }

    private:
      player const& _player;
    };
  }
}

#endif
