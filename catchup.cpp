// mirko.rahn@web.de

#include <board.hpp>
#include <neighbourhood.hpp>

#include <board.cpp>
#include <neighbourhood.cpp>
#include <player.cpp>
#include <point.cpp>

#include <iostream>

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
  neighbourhood<5> const neighbourhood;

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
  neighbourhood<SIZE> const neighbourhood;

  board::board<SIZE> board (neighbourhood.neighbours());

  return winner<SIZE> (&board);
}

int main3()
{
  neighbourhood<3> const neighbourhood;

  board::board<3> board (neighbourhood.neighbours());
  board.put ({4});
  board.put ({5,8});

  return winner<3> (&board);
}

int main()
{
  return main3();
}
