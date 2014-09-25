// mirko.rahn@web.de

#ifndef CATCHUP_BOARD_HPP
#define CATCHUP_BOARD_HPP

#include <neighbourhood.hpp>
#include <hash.hpp>
#include <constant.hpp>
#include <player.hpp>

#include <vector>

namespace
{
  namespace board
  {
    template<int SIZE> class board;
    template<int SIZE> class show;

    template<int SIZE>
    class board
    {
    public:
      typedef table<uint64_t, board<SIZE>, bucket<board<SIZE>, 2>, hash_int> cache_type;

      unsigned long _puts;

      board();

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
      bool is_normal() const;
      void normal (player::player minimum[num_fields (SIZE)]) const;
      unsigned long cache_key() const;

      bool operator== (board<SIZE> const& other) const
      {
        if (_available_stones != other._available_stones)
        {
          return false;
        }
        if (_to_move != other._to_move)
        {
          return false;
        }

        for (int field (0); field < num_fields (SIZE); ++field)
        {
          if (_stone[field] != other._stone[field])
          {
            return false;
          }
        }

        return true;
      }

    private:
      friend class show<SIZE>;

      int _depth;
      int _available_stones;
      player::player _to_move;
      int _high_water;
      player::player _stone[num_fields (SIZE)];
      static neighbourhood<SIZE> N;

      int max_sizes_of_components (std::vector<int> fields) const;
      player::player in_front() const;
    };

    template<int SIZE> std::vector<std::vector<int>> const& translations();
  }
}

#endif
