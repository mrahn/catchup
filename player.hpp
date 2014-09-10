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
}

#endif
