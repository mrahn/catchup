// mirko.rahn@web.de

#include <algorithm>
#include <iostream>
#include <map>
#include <tuple>
#include <vector>

namespace
{
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

  namespace point
  {
    typedef std::tuple<int, int, int> point;

    int x (point const&);
    int y (point const&);
    int z (point const&);
    std::vector<point> plane (int size);
    int distance (point const&, point const&);
    point rotate60 (point const&);
    point mirror (point const&, int axis);
  }

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

  namespace board
  {
    constexpr int num_fields (int size);

    template<int SIZE> class show;

    template<int SIZE>
    class board
    {
    public:
      board (board const&) = delete;
      board operator= (board const&) = delete;
      board (board&&) = delete;
      board operator= (board&&) = delete;

      board();

      void put (std::vector<int>);
      void unput (std::vector<int>, int available_stones, int high_water);
      player::player winner();
      void normal();

    private:
      static std::vector<std::vector<int>> const _neighbour;
      static std::vector<std::vector<int>> const _translations;

      friend class show<SIZE>;

      int _depth;
      int _available_stones;
      player::player _to_move;
      int _high_water;
      player::player _stone[num_fields (SIZE)];

      int max_sizes_of_components (std::vector<int> fields) const;
      player::player in_front() const;
    };
  }

  namespace point
  {
    int x (point const& p) { return std::get<0> (p); }
    int y (point const& p) { return std::get<1> (p); }
    int z (point const& p) { return std::get<2> (p); }

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

  namespace board
  {
    constexpr int num_fields (int size)
    {
      return 3 * size * (size - 1) + (size < 1 ? size : 1);
    }

    template<int SIZE>
    board<SIZE>::board()
      : _depth (0)
      , _available_stones (std::min (num_fields (SIZE), 1))
      , _to_move (player::Blue)
      , _high_water (0)
      , _stone()
    {
      std::fill (_stone, _stone + num_fields (SIZE), player::None);
    }

    template<int SIZE>
    void board<SIZE>::put (std::vector<int> fields)
    {
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
    player::player board<SIZE>::winner()
    {
#define NEXT_FREE_FIELD(_var)                   \
      while (  _var < num_fields (SIZE)         \
            && _stone[_var] != player::None     \
            )                                   \
      {                                         \
        ++_var;                                 \
      }

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

        while (f + 1 < num_fields (SIZE))
        {
          int g (f + 1); NEXT_FREE_FIELD (g);

          while (g < num_fields (SIZE))
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

        while (f < num_fields (SIZE))
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

    template<int SIZE>
    void board<SIZE>::normal()
    {
      player::player minimum[num_fields (SIZE)];

      std::copy (_stone, _stone + num_fields (SIZE), minimum);

      for (std::vector<int> const& translation : _translations)
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

            for (int n : _neighbour[f])
            {
              if (_stone[n] == player && !seen[n])
              {
                stack[top++] = n;
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

            for (int n : _neighbour[f])
            {
              if (_stone[n] == player && !seen[player][n])
              {
                stack[player][top[player]++] = n;
                ++size;
                seen[player][n] = true;
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

      std::vector<std::vector<int>> neighbours (int size)
      {
        std::vector<point::point> const points (point::plane (size));
        std::map<point::point, int> const id_by_point (numbered (points));
        std::vector<std::vector<int>> ns (num_fields (size));
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
    std::vector<std::vector<int>>
    const board<SIZE>::_neighbour {neighbours (SIZE)};

    template<int SIZE>
    std::vector<std::vector<int>>
    const board<SIZE>::_translations {make_translations (SIZE)};

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
  }
}

int main()
{
  board::board<3> board;
  board.put ({4});
  board.put ({5,8});
  board.normal();

  std::cout << board::show<3> (board) << std::endl;

  std::cout << player::show (board.winner()) << std::endl;
}

/*
rahn@brank:~/catchup$ g++ -std=c++11 -Wall -Wextra -O3 -fno-exceptions -pg Catchup.cpp -o Catchup.exe && time ./Catchup.exe
to_move B, high_water 0, stones 1
   . . .
  . . . .
 . . . . .
  . . . .
   . . .
B
4944077214

real	110m51.142s
user	110m48.725s
sys	0m0.080s
*/
