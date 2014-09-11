// mirko.rahn@web.de

#include <board.hpp>
#include <neighbourhood.hpp>

#include <board.cpp>

#include <iostream>

template<int SIZE>
int winner (board::board<SIZE>* board)
{
  std::cout << board::show<SIZE> (*board) << std::endl;

  board->normal();

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
  return lg ({{12},{16,37},{21,31},{22,39},{52,41},{48,20},{15,7},{8,32},{28,44},{23,30},{36,19,14},{45,51},{9,10},{13,29},{6,17,25},{33,34,50},{43,40,47},{53,54}});
}

int main_lg1657875()
{
  return lg ({{30},{22,37},{20,40},{39,13},{46,33},{31,38},{47,45,44},{32,41},{48,49,42},{36,43,50},{24,15,9},{14,8},{3,2,6},{23,16},{10,17,12},{7,1,51},{52,57,29},{19,18},{11,0},{53,58},{28,21},{27,56}});
}

int main_lg1657867()
{
  return lg ({{28},{31,33},{32,48},{16,14},{22,39},{44,12},{13,45},{46,29},{21,23},{38,20,15},{37,53},{24,40},{6,41},{5,0,1},{52,19},{42,49},{11,60},{55,54},{2,7,47},{59,58,57},{51,56},/*{8,3,27},{36,35}*/});
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

int main()
{
  return main_lg1657867();
}
