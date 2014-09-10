// mirko.rahn@web.de

#ifndef CATCHUP_BOARD_HPP
#define CATCHUP_BOARD_HPP

#include <constant.hpp>
#include <player.hpp>

#include <vector>

namespace
{
  namespace board
  {
    template<int SIZE> class show;

    template<int SIZE>
    class board
    {
    public:
      unsigned long _puts;

      board (board const&) = delete;
      board operator= (board const&) = delete;
      board (board&&) = delete;
      board operator= (board&&) = delete;

      board (int const* const neighbours);

      void put (std::vector<int>);
      void unput (std::vector<int>, int available_stones, int high_water);
      player::player winner (int);
      player::player winner() { return winner (0); }
      void normal();

    private:
      friend class show<SIZE>;

      int _depth;
      int _available_stones;
      player::player _to_move;
      int _high_water;
      player::player _stone[num_fields (SIZE)];
      int const* const _neighbours;

      int max_sizes_of_components (std::vector<int> fields) const;
      player::player in_front() const;
    };

    template<int SIZE> std::vector<std::vector<int>> const& translations();
  }
}

#endif
