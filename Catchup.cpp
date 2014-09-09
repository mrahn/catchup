// mirko.rahn@web.de
// clang++ -std=c++11 -Wall -Wextra -O3 -fno-exceptions Catchup.cpp

#include <algorithm>
#include <iostream>
#include <map>
#include <stack>
#include <tuple>
#include <vector>

namespace
{
  namespace point
  {
    typedef std::tuple<int, int, int> point;

    int x (point const& p) { return std::get<0> (p); }
    int y (point const& p) { return std::get<1> (p); }
    int z (point const& p) { return std::get<2> (p); }

    int plane_size (int size)
    {
      return 3 * size * (size - 1) + std::min (1, size);
    }

    std::vector<point> plane (int size)
    {
      std::vector<point> p;

      for (int x (1 - size); x < size; ++x)
      {
        for (int y (1 - size); y < size; ++y)
        {
          for (int z (1 - size); z < size; ++z)
          {
            if (x + y + z == 0)
            {
              p.emplace_back (x, y, z);
            }
          }
        }
      }

      return p;
    }

    int distance (point const& a, point const& b)
    {
      return ( std::abs (x (a) - x (b))
             + std::abs (y (a) - y (b))
             + std::abs (z (a) - z (b))
             ) / 2;
    }

    point rotate60 (point const& p)
    {
      return point ( (2 * x (p) -     y (p) + 2 * z (p)) / 3
                   , (2 * x (p) + 2 * y (p) -     z (p)) / 3
                   , (   -x (p) + 2 * y (p) + 2 * z (p)) / 3
                   );
    }

    point rotate300 (point const& p)
    {
      return point ( (2 * x (p) + 2 * y (p) -     z (p)) / 3
                   , (   -x (p) + 2 * y (p) + 2 * z (p)) / 3
                   , (2 * x (p) -     y (p) + 2 * z (p)) / 3
                   );
    }

    point mirror (point const& p, int k)
    {
      switch (k)
      {
      case 0: return point (x (p), y (p), z (p));
      case 1: return point (x (p), z (p), y (p));
      case 2: return point (y (p), x (p), z (p));
      case 3: return point (y (p), z (p), x (p));
      case 4: return point (z (p), x (p), y (p));
      case 5: return point (z (p), y (p), x (p));
      }

      abort();
    }
  }

  namespace player
  {
    typedef enum {Blue = 0, Orange = 1, None} player;

    player other (player p)
    {
      return (p == Blue) ? Orange : Blue;
    }

    class show
    {
    public:
      show (player const& player)
        : _player (player)
      {}
      std::ostream& operator() (std::ostream& os) const
      {
        return os << ( (_player == Blue) ? 'B'
                     : (_player == Orange) ? 'O'
                     : '.'
                     );
      }

    private:
      player const& _player;
    };
    std::ostream& operator<< (std::ostream& os, show const& s)
    {
      return s (os);
    }
  }

  namespace board
  {
    template<int SIZE> class show;

    template<int SIZE>
    class board
    {
    public:
      board (board const&) = delete;
      board operator= (board const&) = delete;
      board (board&&) = delete;
      board operator= (board&&) = delete;

      board()
        : _depth (0)
        , _available_stones (std::min (point::plane_size (SIZE), 1))
        , _to_move (player::Blue)
        , _high_water (0)
        , _stone (point::plane_size (SIZE), player::None)
      {}

      void put (std::vector<int> fields)
      {
        for (int field : fields)
        {
          _stone[field] = _to_move;
          ++_depth;
        }

        _to_move = other (_to_move);
        int const csize (max_sizes_of_components (fields));
        _available_stones = std::min
          ( point::plane_size (SIZE) - _depth
          , (_high_water > 0 && csize > _high_water && _depth > 1) ? 3 : 2
          );
        _high_water = std::max (_high_water, csize);
      }

      void unput (std::vector<int> fields, int available_stones, int high_water)
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

      player::player winner()
      {
#define NEXT_FREE_FIELD(_var)                                           \
        while (  _var < point::plane_size (SIZE)                        \
              && _stone[_var] != player::None                           \
              )                                                         \
        {                                                               \
          ++_var;                                                       \
        }

        int const available_stones (_available_stones);
        int const high_water (_high_water);

        if (_available_stones > 2)
        {
          int f (0); NEXT_FREE_FIELD (f);

          while (f + 2 < point::plane_size (SIZE))
          {
            int g (f + 1); NEXT_FREE_FIELD (g);

            while (g + 1 < point::plane_size (SIZE))
            {
              int h (g + 1); NEXT_FREE_FIELD (h);

              while (h < point::plane_size (SIZE))
              {
                put ({f, g, h});

                player::player const won (winner());

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

          while (f + 1 < point::plane_size (SIZE))
          {
            int g (f + 1); NEXT_FREE_FIELD (g);

            while (g < point::plane_size (SIZE))
            {
              put ({f, g});

              player::player const won (winner());

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

          while (f < point::plane_size (SIZE))
          {
            put ({f});

            player::player const won (winner());

            unput ({f}, available_stones, high_water);

            if (won == _to_move)
            {
              return _to_move;
            }

            ++f; NEXT_FREE_FIELD (f);
          }
        }

        return _available_stones ? player::other (_to_move) : in_front();

#undef NEXT_FREE_FIELD
      }

      void normal()
      {
        std::vector<player::player> minimum (_stone);

        for (std::vector<int> const& translation : _translations)
        {
          std::vector<player::player> translated (point::plane_size (SIZE));

          bool greater (false);
          bool smaller (false);

          for ( int field (0)
              ; field < point::plane_size (SIZE) && !greater
              ; ++field
              )
          {
            translated[field] = _stone[translation[field]];

            smaller = smaller || translated[field] < minimum[field];
            greater = !smaller && translated[field] > minimum[field];
          }

          if (smaller)
          {
            minimum = translated;
          }
        }

        _stone = minimum;
      }

    private:
      static std::vector<std::vector<int>> const _neighbour;
      static std::vector<std::vector<int>> const _translations;

      friend class show<SIZE>;

      int _depth;
      int _available_stones;
      player::player _to_move;
      int _high_water;
      std::vector<player::player> _stone;

      int max_sizes_of_components (std::vector<int> fields) const
      {
        std::stack<int> stack;
        std::vector<bool> seen (point::plane_size (SIZE), false);
        int max (0);

        for (int field : fields)
        {
          if (!seen[field])
          {
            int size (0);
            player::player const player (_stone[field]);

            stack.push (field);
            ++size;
            seen[field] = true;

            while (!stack.empty())
            {
              int const f (stack.top()); stack.pop();

              for (int n : _neighbour[f])
              {
                if (_stone[n] == player && !seen[n])
                {
                  stack.push (n);
                  ++size;
                  seen[n] = true;
                }
              }
            }

            max = std::max (max, size);
          }
        }

        return max;
      }

      player::player in_front() const
      {
        std::vector<std::stack<int>> stack (2);
        std::vector<std::vector<bool>> seen
          (2, std::vector<bool> (point::plane_size (SIZE), false));
        std::vector<std::vector<int>> sizes (2);

        for (int field (0); field < point::plane_size (SIZE); ++field)
        {
          player::player const player (_stone[field]);

          if (player != player::None && !seen[player][field])
          {
            int size (0);

            stack[player].push (field);
            ++size;
            seen[player][field] = true;

            while (!stack[player].empty())
            {
              int const f (stack[player].top()); stack[player].pop();

              for (int n : _neighbour[f])
              {
                if (_stone[n] == player && !seen[player][n])
                {
                  stack[player].push (n);
                  ++size;
                  seen[player][n] = true;
                }
              }
            }

            sizes[player].emplace_back (size);
          }
        }

        for (player::player player : {player::Blue, player::Orange})
        {
          std::sort
            (sizes[player].begin(), sizes[player].end(), std::greater<int>());
        }

        std::vector<int>::const_iterator b_pos (sizes[player::Blue].begin());
        std::vector<int>::const_iterator o_pos (sizes[player::Orange].begin());

        while (  b_pos != sizes[player::Blue].end()
              && o_pos != sizes[player::Orange].end()
              && *b_pos == *o_pos
              )
        {
          ++b_pos;
          ++o_pos;
        }

        if (  b_pos != sizes[player::Blue].end()
           && o_pos != sizes[player::Orange].end()
           )
        {
          return (*b_pos > *o_pos) ? player::Blue : player::Orange;
        }

        if (b_pos != sizes[player::Blue].end())
        {
          return player::Blue;
        }

        if (o_pos != sizes[player::Orange].end())
        {
          return player::Orange;
        }

        abort();
      }
    };

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

      std::vector<std::vector<int>> neighbours (int size)
      {
        std::vector<point::point> const points (point::plane (size));
        std::map<point::point, int> const id_by_point (numbered (points));
        std::vector<std::vector<int>> ns (points.size());
        int k (0);

        for (point::point const& p : points)
        {
          std::vector<int>& n (ns[k++]);

          for (point::point const& q : points)
          {
            if (point::distance (p, q) == 1)
            {
              n.emplace_back (id_by_point.at (q));
            }
          }
        }

        return ns;
      }

      std::vector<std::vector<int>> make_translations (int size)
      {
        std::vector<point::point> const points (point::plane (size));
        std::map<point::point, int> const id_by_point (numbered (points));

        std::vector<int> rotate60 (point::plane_size (size));

        for (int field (0); field < point::plane_size (size); ++field)
        {
          rotate60[field] = id_by_point.at (point::rotate60 (points[field]));
        }

        std::vector<std::vector<int>> ts
          (12, std::vector<int> (point::plane_size (size)));

        for (int axis (0); axis < 6; ++axis)
        {
          for (int field (0); field < point::plane_size (size); ++field)
          {
            ts[0 + axis][field] =
              id_by_point.at (point::mirror (points[field], axis));

            ts[6 + axis][field] =
              id_by_point.at (point::mirror (points[rotate60[field]], axis));
          }
        }

        return ts;
      }
    }

    template<int SIZE>
    std::vector<std::vector<int>>
    const board<SIZE>::_neighbour {neighbours (SIZE)};

    template<int SIZE>
    std::vector<std::vector<int>>
    const board<SIZE>::_translations {make_translations (SIZE)};

    template<int SIZE>
    class show
    {
    public:
      show (board<SIZE> const& board)
        : _board (board)
      {}
      std::ostream& operator() (std::ostream& os) const
      {
        os << "to_move " << player::show (_board._to_move)
           << ", high_water " << _board._high_water
           << ", stones " << _board._available_stones
           << "\n";

        int line_x (0);
        int k (0);
        int prefix (SIZE - 1);
        int delta (-1);

        auto const print_prefix ([&os, &prefix, &delta]
                                {
                                  os << std::string (prefix, ' ');
                                  prefix += delta;
                                  if (prefix == 0)
                                  {
                                    delta = -delta;
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
    template<int SIZE>
    std::ostream& operator<< (std::ostream& os, show<SIZE> const& s)
    {
      return s (os);
    }
  }
}

int main()
{
  board::board<3> board;

  board.put ({4});
  board.put ({5,8});
  board.put ({6,14});
  // board.put ({9,12});
  //  board.put ({1,2,10});

  std::cout << board::show<3> (board);
  std::cout << player::show (board.winner()) << std::endl;
}
