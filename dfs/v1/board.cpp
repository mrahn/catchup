// mirko.rahn@web.de

#include <board.hpp>

#include <neighbourhood.hpp>
#include <constant.hpp>
#include <player.hpp>
#include <stream_modifier.hpp>

#include <algorithm>
#include <iostream>
#include <map>

namespace
{
  namespace board
  {
    template<int SIZE>
    board<SIZE>::board()
      : _puts (0)
      , _depth (0)
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
    player::player board<SIZE>::winner ( cache_type* won_by_blue
                                       , cache_type* won_by_orange
                                       , int w
                                       )
    {
      unsigned long const key (cache_key());

      if (won_by_blue->has (key, *this))
      {
        return player::Blue;
      }

      if (won_by_orange->has (key, *this))
      {
        return player::Orange;
      }

#define STORE()                                         \
      if (result == player::Blue)                       \
      {                                                 \
        won_by_blue->put (key, *this);                  \
      }                                                 \
      else                                              \
      {                                                 \
        won_by_orange->put (key, *this);                \
      }

#define FOUND_WINNING_MOVE()                    \
      if (result != _to_move)                   \
      {                                         \
        result = _to_move;                      \
                                                \
        STORE();                                \
                                                \
        return result;                          \
      }

#define FINAL_RETURN()                          \
      STORE();                                  \
                                                \
      return result;                            \

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
        if (won == player::other (_to_move))                    \
        {                                                       \
          std::cout << show<SIZE> (*this) << std::endl;         \
        }                                                       \
      }

      player::player result (other (_to_move));

      if (!_available_stones)
      {
        result = in_front();

        FINAL_RETURN();
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

              player::player const won (winner (won_by_blue, won_by_orange, w + 1));

              SHOW();

              unput ({f, g, h}, available_stones, high_water);

              if (won == _to_move)
              {
                FOUND_WINNING_MOVE();
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

            player::player const won (winner (won_by_blue, won_by_orange, w + 1));

            SHOW();

            unput ({f, g}, available_stones, high_water);

            if (won == _to_move)
            {
              FOUND_WINNING_MOVE();
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

          player::player const won (winner (won_by_blue, won_by_orange, w + 1));

          SHOW();

          unput ({f}, available_stones, high_water);

          if (won == _to_move)
          {
            FOUND_WINNING_MOVE();
          }

          ++f; NEXT_FREE_FIELD (f);
        }
      }

      FINAL_RETURN();

#undef SHOW
#undef NEXT_FREE_FIELD
    }

    template<int SIZE>
    bool board<SIZE>::is_normal() const
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
          return false;
        }
      }

      return true;
    }

    template<int SIZE>
    void board<SIZE>::normal (player::player minimum[num_fields (SIZE)]) const
    {
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
    }

    template<int SIZE>
    unsigned long board<SIZE>::cache_key() const
    {
      player::player minimum[num_fields (SIZE)];

      normal (minimum);

      unsigned long key (0);

      key += _to_move; key <<= 1;
      key += _available_stones; key <<= 2;

      int bit (3);

      for (int field (0); field < num_fields (SIZE); ++field, bit += 2)
      {
        if (bit >= 64)
        {
          key = hash_int (key);

          bit = 0;
        }

        key += minimum[field]; key <<= 2;
      }

      return key;
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

            for (int n (N.neighbours[f]); n < N.neighbours[f + 1]; ++n)
            {
              if (_stone[N.neighbours[n]] == player && !seen[N.neighbours[n]])
              {
                stack[top++] = N.neighbours[n];
                ++size;
                seen[N.neighbours[n]] = true;
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

            for (int n (N.neighbours[f]); n < N.neighbours[f + 1]; ++n)
            {
              if (  _stone[N.neighbours[n]] == player
                 && !seen[player][N.neighbours[n]]
                 )
              {
                stack[player][top[player]++] = N.neighbours[n];
                ++size;
                seen[player][N.neighbours[n]] = true;
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
      std::vector<std::vector<int>> make_translations (int size)
      {
        std::vector<std::vector<int>> ts
          (11, std::vector<int> (num_fields (size)));

        int x[num_fields (size)];
        int y[num_fields (size)];
        int z[num_fields (size)];
        int id[num_fields (size)][num_fields (size)][num_fields (size)];

        {
          int p (0);

          for (int px (1 - size); px < size; ++px)
          {
            for (int py (1 - size); py < size; ++py)
            {
              for (int pz (1 - size); pz < size; ++pz)
              {
                if (px + py + pz == 0)
                {
                  x[p] = px;
                  y[p] = py;
                  z[p] = pz;

                  id[px][py][pz] = p;

                  ++p;
                }
              }
            }
          }
        }

        for (int field (0); field < num_fields (size); ++field)
        {
          ts[0][field] = id[x[field]][z[field]][y[field]];
          ts[1][field] = id[y[field]][x[field]][z[field]];
          ts[2][field] = id[y[field]][z[field]][x[field]];
          ts[3][field] = id[z[field]][x[field]][y[field]];
          ts[4][field] = id[z[field]][y[field]][x[field]];

          int const rid (id[(2*x[field] -   y[field] + 2*z[field]) / 3]
                           [(2*x[field] + 2*y[field] -   z[field]) / 3]
                           [( -x[field] + 2*y[field] + 2*z[field]) / 3]
                        );

          ts[5][field] = id[x[rid]][y[rid]][z[rid]];
          ts[6][field] = id[x[rid]][z[rid]][y[rid]];
          ts[7][field] = id[y[rid]][x[rid]][z[rid]];
          ts[8][field] = id[y[rid]][z[rid]][x[rid]];
          ts[9][field] = id[z[rid]][x[rid]][y[rid]];
          ts[10][field] = id[z[rid]][y[rid]][x[rid]];
        }

        return ts;
      }
    }

    template<int SIZE> std::vector<std::vector<int>> const& translations()
    {
      static std::vector<std::vector<int>> const ts {make_translations (SIZE)};

      return ts;
    }

    namespace
    {
      template<int SIZE>
      neighbourhood<SIZE> make_neighbourhood()
      {
        static neighbourhood<SIZE> neighbourhood;

        return neighbourhood;
      }
    }

    template<int SIZE>
    neighbourhood<SIZE> board<SIZE>::N {make_neighbourhood<SIZE>()};

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

        int p (0);

        for (int x (1 - SIZE); x < SIZE; ++x)
        {
          for (int y (1 - SIZE); y < SIZE; ++y)
          {
            for (int z (1 - SIZE); z < SIZE; ++z)
            {
              if (x + y + z == 0)
              {
                if (x + SIZE - 1 != line_x)
                {
                  line_x = x + SIZE - 1;
                  os << "\n";
                  print_prefix();
                }

                os << " " << player::show (_board._stone[p]);

                ++p;
              }
            }
          }
        }

        return os << "\n";
      }

    private:
      board<SIZE> const& _board;
    };
  }
}
