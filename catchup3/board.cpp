// mirko.rahn@web.de

#include <board.hpp>

#include <player.hpp>
#include <stream_modifier.hpp>

#include <algorithm>
#include <iostream>
#include <map>

namespace
{
  namespace board
  {
    board::board (int const* neighbours, int const* translations)
      : _puts (0)
      , _depth (0)
      , _available_stones (1)
      , _to_move (player::Blue)
      , _high_water (0)
      , _stone()
      , _neighbours (neighbours)
      , _translations (translations)
    {
      std::fill (_stone, _stone + 19, player::None);
    }

    void board::put (std::vector<int> fields)
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
        ( 19 - _depth
        , (_high_water > 0 && csize > _high_water && _depth > 1) ? 3 : 2
        );
      _high_water = std::max (_high_water, csize);
    }

    void board::unput
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

    player::player board::winner ( cache_type* won_by_blue
                                 , cache_type* won_by_orange
                                 , int w
                                 )
    {
      unsigned long const key (cache_key());

      if (won_by_blue->count (key))
      {
        return player::Blue;
      }

      if (won_by_orange->count (key))
      {
        return player::Orange;
      }

#define STORE()                                         \
      if (result == player::Blue)                       \
      {                                                 \
        won_by_blue->emplace (key);                     \
      }                                                 \
      else                                              \
      {                                                 \
        won_by_orange->emplace (key);                   \
      }

#define FOUND_WINNING_MOVE()                    \
      if (result != _to_move)                   \
      {                                         \
        result = _to_move;                      \
                                                \
        STORE();                                \
      }

#define FINAL_RETURN()                          \
      STORE();                                  \
                                                \
      return result;                            \

#define NEXT_FREE_FIELD(_var)                   \
      while (  _var < 19                        \
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
          std::cout << show (*this) << std::endl;               \
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

        while (f + 2 < 19)
        {
          int g (f + 1); NEXT_FREE_FIELD (g);

          while (g + 1 < 19)
          {
            int h (g + 1); NEXT_FREE_FIELD (h);

            while (h < 19)
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

        while (f + 1 < 19)
        {
          int g (f + 1); NEXT_FREE_FIELD (g);

          while (g < 19)
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

        while (f < 19)
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

    void board::normal (player::player minimum[19]) const
    {
      std::copy (_stone, _stone + 19, minimum);

      for ( int const* translation (_translations)
          ; translation < _translations + 209
          ; translation += 19
          )
      {
        player::player translated[19];

        bool greater (false);
        bool smaller (false);

        for (int field (0); field < 19 && !greater; ++field)
        {
          translated[field] = _stone[translation[field]];

          smaller = smaller || translated[field] < minimum[field];
          greater = !smaller && translated[field] > minimum[field];
        }

        if (smaller)
        {
          std::copy (translated, translated + 19, minimum);
        }
      }
    }

    unsigned long board::cache_key() const
    {
      player::player minimum[19];

      normal (minimum);

      unsigned long key (0);

      key += _to_move; key <<= 1;
      key += _available_stones; key <<= 2;

      for (int field (0); field < 19; ++field)
      {
        key += minimum[field]; key <<= 2;
      }

      return key;
    }

    int board::max_sizes_of_components (std::vector<int> fields) const
    {
      int stack[19];
      int top (0);
      bool seen[19];
      int max (0);

      std::fill (seen, seen + 19, false);

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

    player::player board::in_front() const
    {
      int stack[2][19];
      int top[2] = {0,0};
      bool seen[2][19];
      int sizes[2][19];
      int pos[2] = {0,0};

      std::fill (seen[0], seen[0] + 19, false);
      std::fill (seen[1], seen[1] + 19, false);
      std::fill (sizes[0], sizes[0] + 19, 0);
      std::fill (sizes[1], sizes[1] + 19, 0);

      for (int field (0); field < 19; ++field)
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

    show::show (board const& board)
      : _board (board)
    {}
    std::ostream& show::operator() (std::ostream& os) const
    {
      return os
        << "to_move " << player::show (_board._to_move)
        << ", high_water " << _board._high_water
        << ", stones " << _board._available_stones << "\n"
        << "  " << player::show (_board._stone[0])
        << " " << player::show (_board._stone[1])
        << " " << player::show (_board._stone[2]) << "\n"
        << " " << player::show (_board._stone[3])
        << " " << player::show (_board._stone[4])
        << " " << player::show (_board._stone[5])
        << " " << player::show (_board._stone[6]) << "\n"
        << player::show (_board._stone[7])
        << " " << player::show (_board._stone[8])
        << " " << player::show (_board._stone[9])
        << " " << player::show (_board._stone[10])
        << " " << player::show (_board._stone[11]) << "\n"
        << " " << player::show (_board._stone[12])
        << " " << player::show (_board._stone[13])
        << " " << player::show (_board._stone[14])
        << " " << player::show (_board._stone[15]) << "\n"
        << "  " << player::show (_board._stone[16])
        << " " << player::show (_board._stone[17])
        << " " << player::show (_board._stone[18])
        << "\n";
    }
  }
}
