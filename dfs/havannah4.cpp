// mirko.jesiak@web.de

#include <algorithm>
#include <array>
#include <cstdint>
#include <iostream>
#include <set>
#include <tuple>
#include <type_traits>
#include <unordered_set>

namespace
{
  enum player {BLUE = 0, ORANGE = 1, NONE = 2};

  std::ostream& operator<< (std::ostream& os, player p)
  {
    return os << (p == player::BLUE ? 'B' : p == player::ORANGE ? 'O' : '.');
  }

  player other (player p)
  {
    return player (1 - p);
  }

  using field = short;

  struct board
  {
    board (board const&) = delete;
    board operator= (board const&) = delete;
    board (board&&) = delete;
    board operator= (board&&) = delete;

    board();

    void put (field);
    void swap();
    void normal();

    std::set<board> suc() const;

    board (player, std::array<player, 37>, field, field);
    board (field);

    template<typename I>
      typename std::enable_if<8 <= sizeof (I), I>::type value() const
    {
      I x (0);

      for (auto pos (_stone.rbegin()); pos != _stone.rend(); ++pos)
      {
        x *= 3;
        x += *pos;
      }

      x <<= 1;
      x += _to_move;

      return x;
    }

    template<typename I>
      board (I x, typename std::enable_if<8 <= sizeof (I), I>::type = {})
        : board()
    {
      _to_move = player (x & 1);
      x >>= 1;

      for (auto& p : _stone)
      {
        p = player (x % 3);
        x /= 3;
        if (p != player::NONE)
        {
          --_free_fields;
        }
      }
    }

  private:
    // field neighbour_begin[38] = {0,3,7,11,14,18,24,30,36,40,44,50,56,62,68,72,75,81,87,93,99,105,108,112,118,124,130,136,140,144,150,156,162,166,169,173,177,180};
    // field neighbour[180] = {1,4,5,0,2,5,6,1,3,6,7,2,7,8,0,5,9,10,0,1,4,6,10,11,1,2,5,7,11,12,2,3,6,8,12,13,3,7,13,14,4,10,15,16,4,5,9,11,16,17,5,6,10,12,17,18,6,7,11,13,18,19,7,8,12,14,19,20,8,13,20,21,9,16,22,9,10,15,17,22,23,10,11,16,18,23,24,11,12,17,19,24,25,12,13,18,20,25,26,13,14,19,21,26,27,14,20,27,15,16,23,28,16,17,22,24,28,29,17,18,23,25,29,30,18,19,24,26,30,31,19,20,25,27,31,32,20,21,26,32,22,23,29,33,23,24,28,30,33,34,24,25,29,31,34,35,25,26,30,32,35,36,26,27,31,36,28,29,34,29,30,33,35,30,31,34,36,31,32,35};

    player _to_move;
    std::array<player, 37> _stone;
    field _free_fields;

    friend std::ostream& operator<< (std::ostream&, board const&);
    friend bool operator< (board const&, board const&);
    // friend bool operator== (board const&, board const&);
  };

  board::board()
    : _to_move (player::BLUE)
    , _stone()
    , _free_fields (37)
  {
    std::fill (_stone.begin(), _stone.end(), player::NONE);
  };

  board::board ( player to_move
               , std::array<player, 37> stone
               , field free_fields
               , field f
               )
    : _to_move (std::move (to_move))
    , _stone (std::move (stone))
    , _free_fields (std::move (free_fields))
  {
    put (f);
    normal();
  }

  board::board (field f)
    : board()
  {
    put (f);
    swap();
  }

  void board::swap()
  {
    for (player& p : _stone)
    {
      if (p != player::NONE)
      {
        p = other (p);
      }
    }
    _to_move = other (_to_move);
  }

  void board::put (field f)
  {
    _stone[f] = _to_move;
    _to_move = other (_to_move);
    --_free_fields;
  }

  static constexpr std::array<field, 407> const translations = {{3,2,1,0,8,7,6,5,4,14,13,12,11,10,9,21,20,19,18,17,16,15,27,26,25,24,23,22,32,31,30,29,28,36,35,34,33,15,22,28,33,9,16,23,29,34,4,10,17,24,30,35,0,5,11,18,25,31,36,1,6,12,19,26,32,2,7,13,20,27,3,8,14,21,21,27,32,36,14,20,26,31,35,8,13,19,25,30,34,3,7,12,18,24,29,33,2,6,11,17,23,28,1,5,10,16,22,0,4,9,15,33,28,22,15,34,29,23,16,9,35,30,24,17,10,4,36,31,25,18,11,5,0,32,26,19,12,6,1,27,20,13,7,2,21,14,8,3,36,32,27,21,35,31,26,20,14,34,30,25,19,13,8,33,29,24,18,12,7,3,28,23,17,11,6,2,22,16,10,5,1,15,9,4,0,15,9,4,0,22,16,10,5,1,28,23,17,11,6,2,33,29,24,18,12,7,3,34,30,25,19,13,8,35,31,26,20,14,36,32,27,21,21,14,8,3,27,20,13,7,2,32,26,19,12,6,1,36,31,25,18,11,5,0,35,30,24,17,10,4,34,29,23,16,9,33,28,22,15,0,4,9,15,1,5,10,16,22,2,6,11,17,23,28,3,7,12,18,24,29,33,8,13,19,25,30,34,14,20,26,31,35,21,27,32,36,3,8,14,21,2,7,13,20,27,1,6,12,19,26,32,0,5,11,18,25,31,36,4,10,17,24,30,35,9,16,23,29,34,15,22,28,33,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0,33,34,35,36,28,29,30,31,32,22,23,24,25,26,27,15,16,17,18,19,20,21,9,10,11,12,13,14,4,5,6,7,8,0,1,2,3}};

  void board::normal()
  {
    std::array<player, 37> minimum (_stone);

    bool is_minimal (true);

    for ( auto translation (translations.begin())
        ; translation < translations.end()
        ; translation += 37
        )
    {
      std::array<player, 37> translated;

      bool greater (false);
      bool smaller (false);

      for (field field (0); field < 37 && !greater; ++field)
      {
        translated[field] = _stone[*(translation + field)];

        smaller = smaller || translated[field] < minimum[field];
        greater = !smaller && translated[field] > minimum[field];
      }

      if (smaller)
      {
        is_minimal = false;

        minimum = translated;
      }
    }

    if (!is_minimal)
    {
      _stone = minimum;
    }
  }

  bool operator< (board const& lhs, board const& rhs)
  {
    return std::make_tuple (lhs._to_move, lhs._stone)
         < std::make_tuple (rhs._to_move, rhs._stone)
      ;
  }
  // bool operator== (board const& lhs, board const& rhs)
  // {
  //   return std::make_tuple (lhs._to_move, lhs._stone)
  //       == std::make_tuple (rhs._to_move, rhs._stone)
  //     ;
  // }

  std::set<board> board::suc() const
  {
    std::set<board> suc;

    for (field field (0); field < 37; ++field)
    {
      if (_stone[field] == player::NONE)
      {
        suc.emplace (_to_move, _stone, _free_fields, field);
      }
      else if (_free_fields == 36)
      {
        suc.emplace (field);
      }
    }

    return suc;
  }

  std::ostream& operator<< (std::ostream& os, board const& b)
  {
#define S(i) ' ' << b._stone[i]

    return os
      << "to_move " << b._to_move
      << " free_fields " << b._free_fields
      << "\n   "             << S( 0) << S( 1) << S( 2) << S( 3)
      << "\n  "         << S( 4) << S( 5) << S( 6) << S( 7) << S( 8)
      << "\n "     << S( 9) << S(10) << S(11) << S(12) << S(13) << S(14)
      << "\n" << S(15) << S(16) << S(17) << S(18) << S(19) << S(20) << S(21)
      << "\n "     << S(22) << S(23) << S(24) << S(25) << S(26) << S(27)
      << "\n  "         << S(28) << S(29) << S(30) << S(31) << S(32)
      << "\n   "             << S(33) << S(34) << S(35) << S(36)
    ;
#undef S
  }

  using game = uint_fast64_t;

  static std::unordered_set<game> seen;

  std::size_t count (board const& b, unsigned depth)
  {
    std::size_t c (1);

    if (depth == 0)
    {
      if (seen.emplace (b.value<game>()).second)
      {
        std::cout << b << '\n';
      }

      return c;
    }

    for (auto const& s : b.suc())
    {
      c += count (s, depth - 1);
    }

    return c;
  }

  // scanl (*) 2 (replicate 36 3)
  static constexpr std::array<game, 38> const q = {{2,6,18,54,162,486,1458,4374,13122,39366,118098,354294,1062882,3188646,9565938,28697814,86093442,258280326,774840978,2324522934,6973568802,20920706406,62762119218,188286357654,564859072962,1694577218886,5083731656658,15251194969974,45753584909922,137260754729766,411782264189298,1235346792567894,3706040377703682,11118121133111046,33354363399333138,100063090197999414,300189270593998242}};

  constexpr player stone (game g, field f)
  {
    return player ((g / q[f]) % 3);
  }

  constexpr game put (game g, field f)
  {
    // game const r (g % q[f]);
    // g /= q[f];
    // g -= player::NONE;
    // g += (r & 1);
    // g *= q[f];
    // g += r;
    // g ^= 1;
    // return g;
    return ((g / q[f] - 2 + (g & 1)) * q[f] + (g % q[f])) ^ 1;
  }
}

int main()
{
  board b;
  unsigned const depth (5);

  std::cout << count (b, depth) << '\n';

  std::cout << seen.size() << '\n';

  // std::cout << b << '\n' << b.value<std::uint_fast64_t>() << '\n';

  // b.put (0);
  // b.put (12);
  // b.put (4);

  // std::cout << b << '\n' << b.value<std::uint_fast64_t>() << '\n';

  // b.swap();

  // std::cout << b << '\n' << b.value<std::uint_fast64_t>() << '\n';

  // board const b2 (b.value<std::uint_fast64_t>());

  // std::cout << b2 << '\n' << b2.value<std::uint_fast64_t>() << '\n';

  // for (field f (0); f < 37; ++f)
  // {
  //   std::cout << stone (b2.value<game>(), f);
  // }
  // std::cout << '\n';

  // board const b3 (put (b2.value<game>(), 18));

  // std::cout << b3 << '\n' << b3.value<game>() << '\n';

  return 0;
}

/*
NO DETECTION OF WINNING POSITIONS

no swap, depth => size with normal / size without normal
0 => 1
1 => 7 / 38
2 => 130 / 1370
3 => 4067 / 47990
4 => 135136 / 1633070
5 => 4435537 / 53940710
6 => 141840966
*/
