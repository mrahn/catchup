// mirko.rahn@web.de

#ifndef CATCHUP_PLAYER_HPP
#define CATCHUP_PLAYER_HPP

#include <stream_modifier.hpp>

namespace
{
  namespace player
  {
    typedef enum {Blue = 0, Orange = 1, None} player;

    player other (player);

    class show : public stream_modifier<player>
    {
    public:
      show (player const&);
      virtual std::ostream& operator() (std::ostream&) const override;

    private:
      player const& _player;
    };
  }

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

#endif
