// mirko.jesiak@web.de

#include <algorithm>
#include <iostream>

namespace
{
  namespace player
  {
    typedef int player;

    constexpr player other (player p) { return 1 - p; }
    constexpr player blue() { return 0; }
    constexpr player orange() { return other (blue()); }

    class show
    {
    public:
      show (bool is_blue, bool is_orange)
        : _showed_player (is_blue ? 'B' : is_orange ? 'O' : '.')
      {}
      show (player player)
        : show (player == blue(), player == orange())
      {}
      std::ostream& operator() (std::ostream& os) const
      {
        return os << _showed_player;
      }
    private:
      char _showed_player;
    };
    std::ostream& operator<< (std::ostream& os, show const& s)
    {
      return s (os);
    }
  }

  class show;

  class board
  {
  public:
    board (board const&) = delete;
    board operator= (board const&) = delete;
    board (board&&) = delete;
    board operator= (board&&) = delete;

    board()
      : _to_move (player::blue())
      , _taken()
    {
      std::fill (_taken[player::blue()], _taken[player::blue()] + 61, false);
      std::fill (_taken[player::orange()], _taken[player::orange()] + 61, false);
    };

    void put (int);
    void put (int, int);
    void put (int, int, int);

  private:
    void unput (int, int high_water);
    void unput (int, int, int high_water);
    void unput (int, int, int, int high_water);

    friend class show;

    int neighbour_begin[62] = {0,3,7,11,15,18,22,28,34,40,46,50,54,60,66,72,78,84,88,92,98,104,110,116,122,128,132,135,141,147,153,159,165,171,177,180,184,190,196,202,208,214,220,224,228,234,240,246,252,258,262,266,272,278,284,290,294,297,301,305,309,312};
    int neighbour[312] = {1,5,6,0,2,6,7,1,3,7,8,2,4,8,9,3,9,10,0,6,11,12,0,1,5,7,12,13, 1,2,6,8,13,14,2,3,7,9,14,15,3,4,8,10,15,16,4,9,16,17,5,12,18,19,5,6,11,13,19,20,6,7,12,14,20,21,7,8,13,15,21,22,8,9,14,16,22,23,9,10,15,17,23,24,10,16,24,25,11,19,26,27,11,12,18,20,27,28,12,13,19,21,28,29,13,14,20,22,29,30,14,15,21,23,30,31,15,16,22,24,31,32,16,17,23,25,32,33,17,24,33,34,18,27,35,18,19,26,28,35,36,19,20,27,29,36,37,20,21,28,30,37,38,21,22,29,31,38,39,22,23,30,32,39,40,23,24,31,33,40,41,24,25,32,34,41,42,25,33,42,26,27,36,43,27,28,35,37,43,44,28,29,36,38,44,45,29,30,37,39,45,46,30,31,38,40,46,47,31,32,39,41,47,48,32,33,40,42,48,49,33,34,41,49,35,36,44,50,36,37,43,45,50,51,37,38,44,46,51,52,38,39,45,47,52,53,39,40,46,48,53,54,40,41,47,49,54,55,41,42,48,55,43,44,51,56,44,45,50,52,56,57,45,46,51,53,57,58,46,47,52,54,58,59,47,48,53,55,59,60,48,49,54,60,50,51,57,51,52,56,58,52,53,57,59,53,54,58,60,54,55,59};
    int translation[671] = {4,3,2,1,0,10,9,8,7,6,5,17,16,15,14,13,12,11,25,24,23,22,21,20,19,18,34,33,32,31,30,29,28,27,26,42,41,40,39,38,37,36,35,49,48,47,46,45,44,43,55,54,53,52,51,50,60,59,58,57,56,26,35,43,50,56,18,27,36,44,51,57,11,19,28,37,45,52,58,5,12,20,29,38,46,53,59,0,6,13,21,30,39,47,54,60,1,7,14,22,31,40,48,55,2,8,15,23,32,41,49,3,9,16,24,33,42,4,10,17,25,34,34,42,49,55,60,25,33,41,48,54,59,17,24,32,40,47,53,58,10,16,23,31,39,46,52,57,4,9,15,22,30,38,45,51,56,3,8,14,21,29,37,44,50,2,7,13,20,28,36,43,1,6,12,19,27,35,0,5,11,18,26,56,50,43,35,26,57,51,44,36,27,18,58,52,45,37,28,19,11,59,53,46,38,29,20,12,5,60,54,47,39,30,21,13,6,0,55,48,40,31,22,14,7,1,49,41,32,23,15,8,2,42,33,24,16,9,3,34,25,17,10,4,60,55,49,42,34,59,54,48,41,33,25,58,53,47,40,32,24,17,57,52,46,39,31,23,16,10,56,51,45,38,30,22,15,9,4,50,44,37,29,21,14,8,3,43,36,28,20,13,7,2,35,27,19,12,6,1,26,18,11,5,0,26,18,11,5,0,35,27,19,12,6,1,43,36,28,20,13,7,2,50,44,37,29,21,14,8,3,56,51,45,38,30,22,15,9,4,57,52,46,39,31,23,16,10,58,53,47,40,32,24,17,59,54,48,41,33,25,60,55,49,42,34,34,25,17,10,4,42,33,24,16,9,3,49,41,32,23,15,8,2,55,48,40,31,22,14,7,1,60,54,47,39,30,21,13,6,0,59,53,46,38,29,20,12,5,58,52,45,37,28,19,11,57,51,44,36,27,18,56,50,43,35,26,0,5,11,18,26,1,6,12,19,27,35,2,7,13,20,28,36,43,3,8,14,21,29,37,44,50,4,9,15,22,30,38,45,51,56,10,16,23,31,39,46,52,57,17,24,32,40,47,53,58,25,33,41,48,54,59,34,42,49,55,60,4,10,17,25,34,3,9,16,24,33,42,2,8,15,23,32,41,49,1,7,14,22,31,40,48,55,0,6,13,21,30,39,47,54,60,5,12,20,29,38,46,53,59,11,19,28,37,45,52,58,18,27,36,44,51,57,26,35,43,50,56,60,59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0,56,57,58,59,60,50,51,52,53,54,55,43,44,45,46,47,48,49,35,36,37,38,39,40,41,42,26,27,28,29,30,31,32,33,34,18,19,20,21,22,23,24,25,11,12,13,14,15,16,17,5,6,7,8,9,10,0,1,2,3,4};

    player::player _to_move;
    bool _taken[2][61];
  };

  class show
  {
  public:
    show (board const& board)
      : _board (board)
    {}
    std::ostream& operator() (std::ostream& os) const
    {
      os << "to_move " << player::show (_board._to_move)
         << "\n";

      auto const line
        ( [this, &os] (int begin, int end)
          {
            for (int field (begin); field < end; ++field)
            {
              os << ' '
                 << player::show ( _board._taken[player::blue()][field]
                                 , _board._taken[player::orange()][field]
                                 );
            }
            os << '\n';
          }
        );

      os << "    "; line (0, 5);
      os << "   "; line (5, 11);
      os << "  "; line (11, 18);
      os << " "; line (18, 26);
      os << ""; line (26, 35);
      os << " "; line (35, 43);
      os << "  "; line (43, 50);
      os << "   "; line (50, 56);
      os << "    "; line (56, 61);

      return os;
    }
  private:
    board const& _board;
  };
  std::ostream& operator<< (std::ostream& os, show const& s)
  {
    return s (os);
  }

  void board::put (int f)
  {
    _taken[_to_move][f] = true;

    _to_move = player::other (_to_move);
  }
  void board::put (int f, int g)
  {
    _taken[_to_move][f] = true;
    _taken[_to_move][g] = true;

    _to_move = player::other (_to_move);
  }
  void board::put (int f, int g, int h)
  {
    _taken[_to_move][f] = true;
    _taken[_to_move][g] = true;
    _taken[_to_move][h] = true;

    _to_move = player::other (_to_move);
  }

  void board::unput (int f)
  {
    _to_move = player::other (_to_move);

    _taken[_to_move][f] = false;
  }
  void board::unput (int f, int g)
  {
    _to_move = player::other (_to_move);

    _taken[_to_move][f] = false;
    _taken[_to_move][g] = false;
  }
  void board::unput (int f, int g, int h)
  {
    _to_move = player::other (_to_move);

    _taken[_to_move][f] = false;
    _taken[_to_move][g] = false;
    _taken[_to_move][h] = false;
  }
}

int main()
{
  board b;
  b.put (0);
  b.put (1, 2);
  b.put (3, 4, 5);

  std::cout << show (b) << std::endl;

  return 0;
}
