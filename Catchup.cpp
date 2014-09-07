
#include <algorithm>
#include <iostream>
#include <map>
#include <stack>
#include <tuple>
#include <unordered_set>
#include <vector>

namespace
{
  namespace point
  {
    typedef std::tuple<int, int, int> point;

    int x (point const& p) { return std::get<0> (p); }
    int y (point const& p) { return std::get<1> (p); }
    int z (point const& p) { return std::get<2> (p); }

    int plane_size (int size)
    {
      return 3 * size * (size - 1) + std::min (1, size);
    }

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

    point rotate300 (point const& p)
    {
      return point ( (2 * x (p) + 2 * y (p) -     z (p)) / 3
                   , (   -x (p) + 2 * y (p) + 2 * z (p)) / 3
                   , (2 * x (p) -     y (p) + 2 * z (p)) / 3
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
    typedef enum {Blue = 0, Orange = 1, None} player;

    player other (player p)
    {
      return (p == Blue) ? Orange : Blue;
    }

    class show
    {
    public:
      show (player const& player)
        : _player (player)
      {}
      std::ostream& operator() (std::ostream& os) const
      {
        return os << ( (_player == Blue) ? 'B'
                     : (_player == Orange) ? 'O'
                     : '.'
                     );
      }

    private:
      player const& _player;
    };
    std::ostream& operator<< (std::ostream& os, show const& s)
    {
      return s (os);
    }
  }

  namespace board
  {
    class show;

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
        std::vector<std::vector<int>> ns (points.size());
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

      std::unordered_set<int> full (int n)
      {
        std::unordered_set<int> s;

        for (int i (0); i < n; ++i)
        {
          s.emplace (i);
        }

        return s;
      }
    }

    class board
    {
    public:
      board (int size)
        : _size (size)
        , _depth (0)
        , _to_move (player::Blue)
        , _high_water (0)
        , _increased_size_of_largest_group (false)
        , _neighbour (neighbours (_size))
        , _free_fields (full (point::plane_size (_size)))
        , _stone (point::plane_size (_size), player::None)
        , _taken (2)
      {}

      void put (std::vector<int> fields)
      {
        for (int field : fields)
        {
          _stone[field] = _to_move;
          _taken[_to_move].emplace_back (field);
          _free_fields.erase (field);
          ++_depth;
        }

        _to_move = other (_to_move);
        std::vector<int> const sizes (sizes_of_components (fields));
        int const csize (*std::max_element (sizes.begin(), sizes.end()));
        _increased_size_of_largest_group
          = (_high_water > 0 && csize > _high_water);
        _high_water = std::max (_high_water, csize);
      }

      player::player in_front() const
      {
        std::vector<int> const b (sizes_of_components_of (player::Blue));
        std::vector<int> const o (sizes_of_components_of (player::Orange));

        std::vector<int>::const_iterator b_pos (b.begin());
        std::vector<int>::const_iterator o_pos (o.begin());

        while (b_pos != b.end() && o_pos != o.end() && *b_pos == *o_pos)
        {
          ++b_pos;
          ++o_pos;
        }

        return
          (b_pos != b.end() && o_pos != o.end()) ? ( (*b_pos > *o_pos)
                                                   ? player::Blue
                                                   : player::Orange
                                                   )
          : (b_pos != b.end())                   ? player::Blue
          : (o_pos != o.end())                   ? player::Orange
          : player::None
          ;
      }

      std::vector<board> successors() const
      {
        std::vector<board> sucs;

        for (int f : _free_fields)
        {
          sucs.emplace_back (*this);

          sucs.back().put ({f});
        }

        if (available_stones() > 1 && _free_fields.size() > 1)
        {
          std::unordered_set<int>::const_iterator f (_free_fields.begin());

          while (std::next (f, 1) != _free_fields.end())
          {
            std::unordered_set<int>::const_iterator g (std::next (f, 1));

            do
            {
              sucs.emplace_back (*this);

              sucs.back().put ({*f, *g});

              ++g;
            }
            while (g != _free_fields.end());

            ++f;
          }
        }

        if (available_stones() > 2 && _free_fields.size() > 2)
        {
          std::unordered_set<int>::const_iterator f (_free_fields.begin());

          while (std::next (f, 2) != _free_fields.end())
          {
            std::unordered_set<int>::const_iterator g (std::next (f, 1));

            while (std::next (g, 1) != _free_fields.end())
            {
              std::unordered_set<int>::const_iterator h (std::next (g, 1));

              do
              {
                sucs.emplace_back (*this);

                sucs.back().put ({*f, *g, *h});

                ++h;
              }
              while (h != _free_fields.end());

              ++g;
            }

            ++f;
          }
        }

        return sucs;
      }

    private:
      friend class show;

      int const _size;
      int _depth;
      player::player _to_move;
      int _high_water;
      bool _increased_size_of_largest_group;
      std::vector<std::vector<int>> const _neighbour;
      std::unordered_set<int> _free_fields;
      std::vector<player::player> _stone;
      std::vector<std::vector<int>> _taken;

      int available_stones() const
      {
        return _depth == 0 ? 1
          : (_increased_size_of_largest_group && _depth > 1) ? 3
          : 2;
      }

      std::vector<int> sizes_of_components_of (player::player player) const
      {
        std::vector<int> sizes (sizes_of_components (_taken[player]));
        std::sort (sizes.begin(), sizes.end(), std::greater<int>());
        return sizes;
      }

      std::vector<int> sizes_of_components (std::vector<int> fields) const
      {
        std::stack<int> stack;
        std::vector<bool> seen (point::plane_size (_size), 0);
        std::vector<int> sizes;

        for (int field : fields)
        {
          if (!seen[field])
          {
            int size (0);
            player::player const player (_stone[field]);

            stack.push (field);
            ++size;
            seen[field] = 1;

            while (!stack.empty())
            {
              int const f (stack.top()); stack.pop();

              for (int n : _neighbour[f])
              {
                if (_stone[n] == player && !seen[n])
                {
                  stack.push (n);
                  ++size;
                  seen[n] = 1;
                }
              }
            }

            sizes.emplace_back (size);
          }
        }

        return sizes;
      }
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
           << ", high_water " << _board._high_water
           << ", stones " << _board.available_stones()
           << "\n";

        int line_x (0);
        int k (0);
        int prefix (_board._size - 1);
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

        for (point::point const& p : point::plane (_board._size))
        {
          int const x (point::x (p) + _board._size - 1);

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
      board const& _board;
    };
    std::ostream& operator<< (std::ostream& os, show const& s)
    {
      return s (os);
    }
  }
}

int main()
{
  board::board b (2);

  b.put ({3});
  b.put ({0,1});

  for (board::board const& s : b.successors())
  {
    std::cout << s;
  }
}
