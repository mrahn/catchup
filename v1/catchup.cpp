// mirko.rahn@web.de

#include <board.hpp>
#include <neighbourhood.hpp>

#include <board.cpp>

#include <iostream>
#include <limits>

template<int SIZE>
int winner (board::board<SIZE>* board)
{
  std::cout << board::show<SIZE> (*board) << std::endl;

  std::cout << player::show (board->winner()) << std::endl;
  std::cout << "Put: " << board->_puts << std::endl;

  return 0;
}

int lg (std::vector<std::vector<int>> const& moves)
{
  neighbourhood<5> const neighbourhood;

  board::board<5> board (neighbourhood.neighbours);

  for (std::vector<int> const& fields : moves)
  {
    board.put (fields);
  }

  return winner<5> (&board);
}

int main_lg1657870()
{
  return lg ({{12},{16,37},{21,31},{22,39},{52,41},{48,20},{15,7},{8,32},{28,44},{23,30},{36,19,14},{45,51},{9,10},{13,29},{6,17,25},{33,34,50},{43,40,47},{53,54},{46,42},{58,57},{24,38,27}});
}

int main_lg1657875()
{
  return lg ({{30},{22,37},{20,40},{39,13},{46,33},{31,38},{47,45,44},{32,41},{48,49,42},{36,43,50},{24,15,9},{14,8},{3,2,6},{23,16},{10,17,12},{7,1,51},{52,57,29},{19,18},{11,0},{53,58},{28,21},{27,56}});
}

int main_lg1657867()
{
  return lg ({{28},{31,33},{32,48},{16,14},{22,39},{44,12},{13,45},{46,29},{21,23},{38,20,15},{37,53},{24,40},{6,41},{5,0,1},{52,19},{42,49},{11,60},{55,54},{2,7,47},{59,58,57},{51,56},{8,3,27},{36,35}});
}

int main_lg1657873()
{
  return lg ({{23},{31,47},{30,46},{38,21},{14,20},{28,15},{29,39},{40,22,13},{53,54,7},{10,5},{36,49},{37,33},{44,6},{12,19},{32,41,48},{52,17},{16,9},{8,3,45},{51,57,58},/*{4,27,35},{43,24,25},{0,1},{2,59,34},{18,11},{42,55,60},{26,50,56}*/});
}

template<int SIZE>
int main_full()
{
  neighbourhood<SIZE> const neighbourhood;

  board::board<SIZE> board (neighbourhood.neighbours);

  return winner<SIZE> (&board);
}

int main3()
{
  neighbourhood<3> const neighbourhood;

  board::board<3> board (neighbourhood.neighbours);
  board.put ({4});
  board.put ({5,8});

  return winner<3> (&board);
}

template<int SIZE>
int main_full2()
{
  neighbourhood<SIZE> const neighbourhood;

  board::board<SIZE> board (neighbourhood.neighbours);

  player::player won [num_fields (SIZE)];
  int lost[num_fields (SIZE)];
  int lost_pos[2][num_fields (SIZE)];
  int suc[num_fields (SIZE)];
  unsigned long sum_puts (0);
  unsigned long max_puts (0);
  unsigned long min_puts (std::numeric_limits<unsigned long>::max());

  std::fill (won, won + num_fields (SIZE), player::Blue);
  std::fill (lost, lost + num_fields (SIZE), 0);
  std::fill (lost_pos[0], lost_pos[0] + num_fields (SIZE), 0);
  std::fill (lost_pos[1], lost_pos[1] + num_fields (SIZE), 0);
  std::fill (suc, suc + num_fields (SIZE), 0);

  auto const calculate
    ([&](int b, int o1, int o2)
    {
      std::cout << board::show<SIZE> (board) << std::endl;
      board._puts = 0;
      player::player const w (board.winner());
      sum_puts += board._puts;
      max_puts = std::max (max_puts, board._puts);
      min_puts = std::min (min_puts, board._puts);
      std::cout << "* * * * " << b << " " << o1 << " " << o2 << ":"
                << " " << player::show (w)
                << " puts " << board._puts
                << " min " << min_puts
                << " max " << max_puts
                << " sum " << sum_puts
                << std::endl;
      if (w == player::Orange)
      {
        won[b] = player::Orange;
        lost[b] = suc[b];
        lost_pos[0][b] = o1;
        lost_pos[1][b] = o2;
      }
      ++suc[b];
    }
    );

  for (int b (0); b < num_fields (SIZE); ++b)
  {
    board.put ({b});

    if (board.is_normal())
    {
      for (int o1 (0); o1 < num_fields (SIZE) && won[b] == player::Blue; ++o1)
      {
        if (o1 != b)
        {
          for ( int o2 (o1 + 1)
              ; o2 < num_fields (SIZE) && won[b] == player::Blue
              ; ++o2
              )
          {
            if (o2 != b)
            {
              board.put ({o1,o2});

              calculate (b, o1, o2);

              board.unput ({o1,o2}, 2, 1);
            }
          }

          if (won[b] == player::Blue)
          {
            board.put ({o1});

            calculate (b, o1, -1);

            board.unput ({o1}, 2, 1);
          }
        }
      }
    }

    board.unput ({b}, 1, 0);
  }

  std::cout << "SUMMARY" << std::endl;

  for (int b (0); b < num_fields (SIZE); ++b)
  {
    if (suc[b])
    {
      std::cout << b << ": " << player::show (won[b]);

      if (won[b] == player::Orange)
      {
        std::cout << " " << lost[b]
                  << " {" << lost_pos[0][b] << "," << lost_pos[1][b] << "}";
      }

      std::cout << std::endl;
    }
  }

  std::cout << "puts:"
            << " min " << min_puts
            << " max " << max_puts
            << " sum " << sum_puts
            << std::endl;

  return 0;
}

int main()
{
  return main_full2<3>();
}
