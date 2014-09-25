// mirko.rahn@web.de

#ifndef CATCHUP_NEIGHBOURHOOD_HPP
#define CATCHUP_NEIGHBOURHOOD_HPP

#include <constant.hpp>
#include <cstdlib>

namespace
{
  template<int SIZE>
  struct neighbourhood
  {
    neighbourhood()
    {
      int k (num_fields (SIZE) + 1);
      int p (0);

      for (int px (1 - SIZE); px < SIZE; ++px)
      {
        for (int py (1 - SIZE); py < SIZE; ++py)
        {
          for (int pz (1 - SIZE); pz < SIZE; ++pz)
          {
            if (px + py + pz == 0)
            {
              neighbours[p] = k;

              int q (0);

              for (int qx (1 - SIZE); qx < SIZE; ++qx)
              {
                for (int qy (1 - SIZE); qy < SIZE; ++qy)
                {
                  for (int qz (1 - SIZE); qz < SIZE; ++qz)
                  {
                    if (qx + qy + qz == 0)
                    {
                      if (( std::abs (px - qx)
                          + std::abs (py - qy)
                          + std::abs (pz - qz)
                          ) == 2
                         )
                      {
                        neighbours[k++] = q;
                      }

                      ++q;
                    }
                  }
                }
              }

              ++p;
            }
          }
        }
      }

      neighbours[p] = k;
    }

    int neighbours[num_fields (SIZE) + 1 + num_neighbours (SIZE)];
  };
}

#endif
