// mirko.rahn@web.de

#include <board.hpp>

#include <board.cpp>

#include <iostream>
#include <fstream>

int main()
{
  int neighbours[104] {20,23,27,30,34,40,46,50,53,59,65,71,74,78,84,90,94,97,101,104,1,3,4,0,2,4,5,1,5,6,0,4,7,8,0,1,3,5,8,9,1,2,4,6,9,10,2,5,10,11,3,8,12,3,4,7,9,12,13,4,5,8,10,13,14,5,6,9,11,14,15,6,10,15,7,8,13,16,8,9,12,14,16,17,9,10,13,15,17,18,10,11,14,18,12,13,17,13,14,16,18,14,15,17};
  int translations[209] {2,1,0,6,5,4,3,11,10,9,8,7,15,14,13,12,18,17,16
                        ,7,12,16,3,8,13,17,0,4,9,14,18,1,5,10,15,2,6,11
                        ,11,15,18,6,10,14,17,2,5,9,13,16,1,4,8,12,0,3,7
                        ,16,12,7,17,13,8,3,18,14,9,4,0,15,10,5,1,11,6,2
                        ,18,15,11,17,14,10,6,16,13,9,5,2,12,8,4,1,7,3,0
                        ,7,3,0,12,8,4,1,16,13,9,5,2,17,14,10,6,18,15,11
                        ,11,6,2,15,10,5,1,18,14,9,4,0,17,13,8,3,16,12,7
                        ,0,3,7,1,4,8,12,2,5,9,13,16,6,10,14,17,11,15,18
                        ,2,6,11,1,5,10,15,0,4,9,14,18,3,8,13,17,7,12,16
                        ,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0
                        ,16,17,18,12,13,14,15,7,8,9,10,11,3,4,5,6,0,1,2
                        };

  board::board board (neighbours, translations);

  cache_type won_by_blue;
  cache_type won_by_orange;

  std::cout << "reading cache..." << std::endl;

  {
    std::ifstream blue ("blue.dat");

    while (blue)
    {
      unsigned long v;

      blue >> v;

      won_by_blue.emplace (v);
    }
  }

  {
    std::ifstream orange ("orange.dat");

    while (orange)
    {
      unsigned long v;

      orange >> v;

      won_by_orange.emplace (v);
    }
  }

  std::size_t const blues (won_by_blue.size());
  std::size_t const oranges (won_by_orange.size());

  std::cout << board::show (board) << std::endl;
  std::cout << player::show (board.winner (&won_by_blue, &won_by_orange))
            << std::endl;
  std::cout << "Put: " << board._puts << std::endl;
  std::cout << "Won_by_blue: " << won_by_blue.size() << std::endl;
  std::cout << "Won_by_orange: " << won_by_orange.size() << std::endl;

  std::cout << "writing cache..." << std::endl;

  if (won_by_blue.size() > blues)
  {
    std::ofstream blue ("blue.dat");

    for (unsigned long i : won_by_blue)
    {
      blue << i << std::endl;
    }
  }

  if (won_by_orange.size() > oranges)
  {
    std::ofstream orange ("orange.dat");

    for (unsigned long i : won_by_orange)
    {
      orange << i << std::endl;
    }
  }

  return 0;
}
