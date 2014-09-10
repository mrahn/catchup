// mirko.rahn@web.de

#include <stream_modifier.hpp>
#include <point.hpp>
#include <player.hpp>

#include <point.cpp>
#include <player.cpp>

#include <algorithm>
#include <iostream>
#include <map>
#include <tuple>
#include <vector>

namespace
{
  namespace board
  {
    constexpr int num_fields (int size);
    constexpr int num_neighbours (int size);

    template<int SIZE> class show;

    template<int SIZE>
    class neighbourhood
    {
    public:
      neighbourhood();
      int const* neighbours() const;
    private:
      int _neighbours[num_fields (SIZE) + 1 + num_neighbours (SIZE)];
    };

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

  namespace board
  {
    constexpr int num_fields (int size)
    {
      return 3 * size * (size - 1) + (size < 1 ? size : 1);
    }
    constexpr int num_neighbours (int size)
    {
      return (size < 1) ? 0 : 6 * (size - 1) * (3 * size - 2);
    }

    template<int SIZE>
    board<SIZE>::board (int const* neighbours)
      : _puts (0)
      , _depth (0)
      , _available_stones (std::min (num_fields (SIZE), 1))
      , _to_move (player::Blue)
      , _high_water (0)
      , _stone()
      , _neighbours (neighbours)
    {
      std::fill (_stone, _stone + num_fields (SIZE), player::None);
    }

    template<int SIZE>
    void board<SIZE>::put (std::vector<int> fields)
    {
      ++_puts;

      for (int field : fields)
      {
        _stone[field] = _to_move;
        ++_depth;
      }

      _to_move = other (_to_move);
      int const csize (max_sizes_of_components (fields));
      _available_stones = std::min
        ( num_fields (SIZE) - _depth
        , (_high_water > 0 && csize > _high_water && _depth > 1) ? 3 : 2
        );
      _high_water = std::max (_high_water, csize);
    }

    template<int SIZE>
    void board<SIZE>::unput
      (std::vector<int> fields, int available_stones, int high_water)
    {
      for (int field : fields)
      {
        _stone[field] = player::None;
        --_depth;
      }

      _to_move = other (_to_move);
      _available_stones = available_stones;
      _high_water = high_water;
    }

    template<int SIZE>
    player::player board<SIZE>::winner (int w)
    {
#define NEXT_FREE_FIELD(_var)                   \
      while (  _var < num_fields (SIZE)         \
            && _stone[_var] != player::None     \
            )                                   \
      {                                         \
        ++_var;                                 \
      }
#define SHOW()                                                  \
      if (w == 0)                                               \
      {                                                         \
        std::cout << "* " << c++ << " " << _puts << std::endl;  \
                                                                \
        if (won == player::other (_to_move))                    \
        {                                                       \
          std::cout << show<SIZE> (*this) << std::endl;         \
        }                                                       \
      }

      int c (0);
      int const available_stones (_available_stones);
      int const high_water (_high_water);

      if (_available_stones > 2)
      {
        int f (0); NEXT_FREE_FIELD (f);

        while (f + 2 < num_fields (SIZE))
        {
          int g (f + 1); NEXT_FREE_FIELD (g);

          while (g + 1 < num_fields (SIZE))
          {
            int h (g + 1); NEXT_FREE_FIELD (h);

            while (h < num_fields (SIZE))
            {
              put ({f, g, h});

              player::player const won (winner (w + 1));

              SHOW();

              unput ({f, g, h}, available_stones, high_water);

              if (won == _to_move)
              {
                return _to_move;
              }

              ++h; NEXT_FREE_FIELD (h);
            }

            ++g; NEXT_FREE_FIELD (g);
          }

          ++f; NEXT_FREE_FIELD (f);
        }
      }

      if (_available_stones > 1)
      {
        int f (0); NEXT_FREE_FIELD (f);

        while (f + 1 < num_fields (SIZE))
        {
          int g (f + 1); NEXT_FREE_FIELD (g);

          while (g < num_fields (SIZE))
          {
            put ({f, g});

            player::player const won (winner (w + 1));

            SHOW();

            unput ({f, g}, available_stones, high_water);

            if (won == _to_move)
            {
              return _to_move;
            }

            ++g; NEXT_FREE_FIELD (g);
          }

          ++f; NEXT_FREE_FIELD (f);
        }
      }

      {
        int f (0); NEXT_FREE_FIELD (f);

        while (f < num_fields (SIZE))
        {
          put ({f});

          player::player const won (winner (w + 1));

          SHOW();

          unput ({f}, available_stones, high_water);

          if (won == _to_move)
          {
            return _to_move;
          }

          ++f; NEXT_FREE_FIELD (f);
        }
      }

      return _available_stones ? player::other (_to_move) : in_front();

#undef SHOW
#undef NEXT_FREE_FIELD
    }

    template<int SIZE>
    void board<SIZE>::normal()
    {
      player::player minimum[num_fields (SIZE)];

      std::copy (_stone, _stone + num_fields (SIZE), minimum);

      for (std::vector<int> const& translation : translations<SIZE>())
      {
        player::player translated[num_fields (SIZE)];

        bool greater (false);
        bool smaller (false);

        for (int field (0); field < num_fields (SIZE) && !greater; ++field)
        {
          translated[field] = _stone[translation[field]];

          smaller = smaller || translated[field] < minimum[field];
          greater = !smaller && translated[field] > minimum[field];
        }

        if (smaller)
        {
          std::copy (translated, translated + num_fields (SIZE), minimum);
        }
      }

      std::copy (minimum, minimum + num_fields (SIZE), _stone);
    }

    template<int SIZE>
    int board<SIZE>::max_sizes_of_components (std::vector<int> fields) const
    {
      int stack[num_fields (SIZE)];
      int top (0);
      bool seen[num_fields (SIZE)];
      int max (0);

      std::fill (seen, seen + num_fields (SIZE), false);

      for (int field : fields)
      {
        if (!seen[field])
        {
          int size (0);
          player::player const player (_stone[field]);

          stack[top++] = field;
          ++size;
          seen[field] = true;

          while (top > 0)
          {
            int const f (stack[--top]);

            for (int n (_neighbours[f]); n < _neighbours[f + 1]; ++n)
            {
              if (_stone[_neighbours[n]] == player && !seen[_neighbours[n]])
              {
                stack[top++] = _neighbours[n];
                ++size;
                seen[_neighbours[n]] = true;
              }
            }
          }

          max = std::max (max, size);
        }
      }

      return max;
    }

    template<int SIZE>
    player::player board<SIZE>::in_front() const
    {
      int stack[2][num_fields (SIZE)];
      int top[2] = {0,0};
      bool seen[2][num_fields (SIZE)];
      int sizes[2][num_fields (SIZE)];
      int pos[2] = {0,0};

      std::fill (seen[0], seen[0] + num_fields (SIZE), false);
      std::fill (seen[1], seen[1] + num_fields (SIZE), false);
      std::fill (sizes[0], sizes[0] + num_fields (SIZE), 0);
      std::fill (sizes[1], sizes[1] + num_fields (SIZE), 0);

      for (int field (0); field < num_fields (SIZE); ++field)
      {
        player::player const player (_stone[field]);

        if (player != player::None && !seen[player][field])
        {
          int size (0);

          stack[player][top[player]++] = field;
          ++size;
          seen[player][field] = true;

          while (top[player] > 0)
          {
            int const f (stack[player][--top[player]]);

            for (int n (_neighbours[f]); n < _neighbours[f + 1]; ++n)
            {
              if (  _stone[_neighbours[n]] == player
                 && !seen[player][_neighbours[n]]
                 )
              {
                stack[player][top[player]++] = _neighbours[n];
                ++size;
                seen[player][_neighbours[n]] = true;
              }
            }
          }

          sizes[player][pos[player]++] = size;
        }
      }

      for (player::player player : {player::Blue, player::Orange})
      {
        std::sort
          (sizes[player], sizes[player] + pos[player], std::greater<int>());
      }

      int b_pos (0);
      int o_pos (0);

      while (  b_pos != pos[player::Blue]
            && o_pos != pos[player::Orange]
            && sizes[player::Blue][b_pos] == sizes[player::Orange][o_pos]
            )
      {
        ++b_pos;
        ++o_pos;
      }

      if (  b_pos != pos[player::Blue]
         && o_pos != pos[player::Orange]
         )
      {
        return (sizes[player::Blue][b_pos] > sizes[player::Orange][o_pos])
          ? player::Blue : player::Orange;
      }

      if (b_pos != pos[player::Blue])
      {
        return player::Blue;
      }

      if (o_pos != pos[player::Orange])
      {
        return player::Orange;
      }

      abort();
    }

    namespace
    {
      std::map<point::point, int>
        numbered (std::vector<point::point> const& ps)
      {
        std::map<point::point, int> m;
        int k (0);

        for (point::point const& p : ps)
        {
          m.emplace (p, k++);
        }

        return m;
      }

      std::vector<std::vector<int>> make_translations (int size)
      {
        std::vector<point::point> const points (point::plane (size));
        std::map<point::point, int> const id_by_point (numbered (points));

        std::vector<std::vector<int>> ts
          (11, std::vector<int> (num_fields (size)));

        for (int axis (1); axis < 6; ++axis)
        {
          for (int field (0); field < num_fields (size); ++field)
          {
            ts[axis - 1][field] =
              id_by_point.at (point::mirror (points[field], axis));
          }
        }

        for (int axis (0); axis < 6; ++axis)
        {
          for (int field (0); field < num_fields (size); ++field)
          {
            ts[axis + 5][field] =
              id_by_point.at (point::mirror (points[id_by_point.at (point::rotate60 (points[field]))], axis));
          }
        }

        return ts;
      }
    }

    template<int SIZE>
    neighbourhood<SIZE>::neighbourhood()
      : _neighbours()
    {
      std::vector<point::point> const points (point::plane (SIZE));
      std::map<point::point, int> const id_by_point (numbered (points));

      int k (num_fields (SIZE) + 1);
      int f (0);

      for (point::point const& p : points)
      {
        _neighbours[f++] = k;

        for (point::point const& q : points)
        {
          if (point::distance (p, q) == 1)
          {
            _neighbours[k++] = id_by_point.at (q);
          }
        }
      }

      _neighbours[f] = k;
    }
    template<int SIZE>
    int const* neighbourhood<SIZE>::neighbours() const
    {
      return _neighbours;
    }

    template<int SIZE> std::vector<std::vector<int>> const& translations()
    {
      static std::vector<std::vector<int>> const ts {make_translations (SIZE)};

      return ts;
    }

    template<int SIZE>
    class show : public stream_modifier<board<SIZE>>
    {
    public:
      show (board<SIZE> const& board)
        : _board (board)
      {}
      virtual std::ostream& operator() (std::ostream& os) const override
      {
        os << "to_move " << player::show (_board._to_move)
           << ", high_water " << _board._high_water
           << ", stones " << _board._available_stones
           << "\n";

        int line_x (0);
        int k (0);
        std::size_t prefix (SIZE - 1);
        bool shrink (true);

        auto const print_prefix ([&os, &prefix, &shrink]
                                {
                                  os << std::string (prefix, ' ');
                                  prefix = shrink ? prefix - 1 : prefix + 1;
                                  if (prefix == 0)
                                  {
                                    shrink = false;
                                  }
                                }
                                );

        print_prefix();

        for (point::point const& p : point::plane (SIZE))
        {
          int const x (point::x (p) + SIZE - 1);

          if (x != line_x)
          {
            line_x = x;
            os << "\n";
            print_prefix();
          }

          os << " " << player::show (_board._stone[k]);

          ++k;
        }

        return os << "\n";
      }

    private:
      board<SIZE> const& _board;
    };
  }
}

template<int SIZE>
int winner (board::board<SIZE>* board)
{
  std::cout << board::show<SIZE> (*board) << std::endl;

  std::cout << player::show (board->winner()) << std::endl;
  std::cout << "Put: " << board->_puts << std::endl;

  return 0;
}

int main_lg1657870()
{
  board::neighbourhood<5> const neighbourhood;

  board::board<5> board (neighbourhood.neighbours());

  board.put ({12});
  board.put ({16,37});
  board.put ({21,31});
  board.put ({22,39});
  board.put ({52,41});
  board.put ({48,20});
  board.put ({15,7});
  board.put ({8,32});
  board.put ({28,44});
  board.put ({23,30});
  board.put ({36,19,14});
  board.put ({45,51});
  board.put ({9,10});
  board.put ({13,29});
  board.put ({6,17,25});
  board.put ({33,34,50});
  board.put ({43,40,47});

  return winner<5> (&board);
}

template<int SIZE>
int main_full()
{
  board::neighbourhood<SIZE> const neighbourhood;

  board::board<SIZE> board (neighbourhood.neighbours());

  return winner<SIZE> (&board);
}

int main3()
{
  board::neighbourhood<3> const neighbourhood;

  board::board<3> board (neighbourhood.neighbours());
  board.put ({4});
  board.put ({5,8});

  return winner<3> (&board);
}

int main()
{
  return main3();
}
