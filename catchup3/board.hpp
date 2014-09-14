// mirko.rahn@web.de

#ifndef CATCHUP3_BOARD_HPP
#define CATCHUP3_BOARD_HPP

#include <hash.hpp>

#include <player.hpp>

#include <vector>

namespace
{
  typedef table<uint64_t, uint64_t, bucket<uint64_t, 1>, hash_int> cache_type;

  namespace board
  {
    class show;

    class board
    {
    public:
      unsigned long _puts;

      board (board const&) = delete;
      board operator= (board const&) = delete;
      board (board&&) = delete;
      board operator= (board&&) = delete;

      board (int const* const neighbours, int const* const translations);

      void put (std::vector<int>);
      void unput (std::vector<int>, int available_stones, int high_water);
      player::player winner ( cache_type* won_by_blue
                            , cache_type* won_by_orange
                            , int w
                            );
      player::player winner ( cache_type* won_by_blue
                            , cache_type* won_by_orange
                            )
      {
        return winner (won_by_blue, won_by_orange, 0);
      }
      void normal (player::player minimum[19]) const;
      unsigned long cache_key() const;

    private:
      friend class show;

      int _depth;
      int _available_stones;
      player::player _to_move;
      int _high_water;
      player::player _stone[19];
      int const* const _neighbours;
      int const* const _translations;

      int max_sizes_of_components (std::vector<int> fields) const;
      player::player in_front() const;
    };

    class show : public stream_modifier<board>
    {
    public:
      show (board const& );
      virtual std::ostream& operator() (std::ostream& os) const override;

    private:
      board const& _board;
    };
  }
}

#endif
