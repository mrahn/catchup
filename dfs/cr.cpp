#include <algorithm>
#include <array>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <iostream>
#include <optional>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

namespace
{
  void error (std::string what)
  {
    std::cout << what << '\n';

    exit (EXIT_FAILURE);
  }

  class pairs
  {
  public:
    std::vector<std::pair<int,int>> const& operator()() const
    {
      return _pairs;
    }
    int max() const
    {
      return _max + 1;
    }

    pairs (char* input)
      : _input (input)
    {
      consume ('[');

      while (not end())
      {
        optional (',');

        auto const p {pair()};

        if (!p)
        {
          break;
        }

        _pairs.emplace_back (p.value());
      }

      consume (']');

      std::sort (_pairs.begin(), _pairs.end());
    }

  private:
    void parse_error (std::string what) const
    {
      error ("Parse error: " + what);
    }

    bool end()
    {
      return _input == nullptr || *_input == '\0';
    }

    char consume()
    {
      return *_input++;
    }

    void expect (char x)
    {
      if (end() || *_input != x)
      {
        parse_error ("expected '" + std::string (1, x) + "'");
      }
    }

    void consume (char x)
    {
      expect (x); consume();
    }

    bool is_digit()
    {
      return not end() && std::isdigit (*_input);
    }

    void expect_digit()
    {
      if (not is_digit())
      {
        parse_error ("expected DIGIT");
      }
    }

    void optional (char x)
    {
      if (not end() && *_input == x)
      {
        consume();
      }
    }

    int value()
    {
      return consume() - '0';
    }

    int num()
    {
      expect_digit();

      int x {value()};

      while (is_digit())
      {
        x *= 10;
        x += value();
      }

      _max = std::max (_max, x);

      return x;
    }

    std::optional<std::pair<int,int>> pair()
    {
      if (end() || *_input != '(')
      {
        return {};
      }
      consume();
      int const l {num()};
      consume (',');
      int const r {num()};
      consume (')');
      return std::pair<int,int> {l,r};
    }

    char* _input;
    std::vector<std::pair<int,int>> _pairs;
    int _max = -1;
  };

  class neighbours
  {
  public:
    neighbours (char* input)
      : _pairs (input)
      , _ns (_pairs().size())
      , _begin (count() + 1)
    {
      int x {0};
      int pos {0};
      _begin[x] = pos;
      _begin[_begin.size() - 1] = _ns.size();

      for (auto p : _pairs())
      {
        _ns[pos] = p.second;

        if (p.first != x)
        {
          _begin[++x] = pos;
        }

        ++pos;
      }
    }
    int count() const
    {
      return _pairs.max();
    }
    int count (int n) const
    {
      assert (n < count());

      return _begin[n+1] - _begin[n];
    }
    int operator() (int n, int k) const
    {
      assert (n < count());
      assert (k < count (n));

      return _ns[_begin[n] + k];
    }

  private:
    pairs const _pairs;
    std::vector<int> _ns;
    std::vector<int> _begin;
  };

  unsigned long count_calls_to_winner {0};
  unsigned long count_puts {0};
  unsigned long count_hits {0};
  unsigned long count_gets {0};
  unsigned long count_leafs {0};
  unsigned long count_score_other {0};
  unsigned long count_score_player {0};

  // class entry
  // {
  // public:
  //   entry()
  //     : _data {-1, -1, -1, -1, -1, -1}
  //   {}
  //   void set (int available, int player, int result)
  //   {
  //     int& val {_data[pos (available, player)]};

  //     assert (val == -1 || val == result);

  //     val = result;
  //   }
  //   int get (int available, int player) const
  //   {
  //     return _data[pos (available, player)];
  //   }
  // private:
  //   constexpr int pos (int available, int player) const
  //   {
  //     return 3 * player + available - 1;
  //   }
  //   std::array<int, 6> _data;
  // };

  // class entry
  // {
  // public:
  //   entry()
  //     : _data {0b111111111111}
  //   {}
  //   void set (int available, int player, int result)
  //   {
  //     _data &= ~((result == 0 ? 0b01 : 0b10) << pos (available, player));
  //   }
  //   int get (int available, int player) const
  //   {
  //     return (_data >> pos (available, player)) & 0b11;
  //   }
  // private:
  //   constexpr int pos (int available, int player) const
  //   {
  //     return 2 * (3 * player + available - 1);
  //   }
  //   std::uint16_t _data;
  // };

  class storage
  {
  public:
    void put (std::uint64_t key, int available, int player, int result)
    {
      ++count_puts;

      _entries.emplace (key, 0b111111111111).first
        ->second &= ~((result == 0 ? 0b01 : 0b10) << shift (available, player));
    }
    int get (std::uint64_t key, int available, int player) const
    {
      auto const pos {_entries.find (key)};

      if (pos == _entries.end())
      {
        return 0b11;
      }

      return (pos->second >> shift (available, player)) & 0b11;
    }
  private:
    std::unordered_map<std::uint64_t, std::uint16_t> _entries;
    constexpr int shift (int available, int player) const
    {
      return 2 * (3 * player + available - 1);
    }
  };

#ifndef NDEBUG
#define NAME(...) __VA_ARGS__
#else
#define NAME(...)
#endif

  class memory
  {
  public:
    memory (int NAME (k))
      : _ {0}
    {
      assert (k <= 32);
    }
    bool seen (int k) const
    {
      return (_ & (1 << k)) > 0;
    }
    void see (int k)
    {
      _ |= (1 << k);
    }
  private:
    std::uint32_t _;
  };

#define TAKEN(p,k) ((_val[p] & (1 << (k))) > 0)
#define EMPTY(k) (!TAKEN (0, k) && !TAKEN (1, k))

  class board
  {
  public:
    board (neighbours ns, int k, storage& storage)
      : _ns (std::move (ns))
      , _score {1, 0}
      , _player (0)
      , _available (2)
      , _free (_ns.count() - 1)
      , _val {0, 0}
      , _storage (storage)
      , _open (_ns.count())
    {
      assert (k < _ns.count());

      _val[_player] |= (1 << k);
      _player = 1 - _player;
    }
    int size_of_component (memory& memory, int player, int k) const
    {
      int s {0};
      int pos {0};

      assert (TAKEN (player, k));

      if (!memory.seen (k))
      {
        _open[pos++] = k;
        memory.see (k);
        ++s;
      }

      while (pos > 0)
      {
        int const f {_open[--pos]};
        for (int k {0}; k < _ns.count (f); ++k)
        {
          int const n (_ns (f, k));
          if (!memory.seen (n) && TAKEN (player, n))
          {
            _open[pos++] = n;
            memory.see (n);
            ++s;
          }
        }
      }

      return s;
    }
    int winner()
    {
      ++count_calls_to_winner;

      if (_score[1 - _player] > _free + _score[_player])
      {
        ++count_score_other;

        return 1 - _player;
      }

      if (_score[_player] > _free + _score[1 - _player])
      {
        ++count_score_player;

        return _player;
      }

      if (_free == 0)
      {
        ++count_leafs;

        std::array<std::vector<int>, 2> sizes;
        std::array<memory, 2> memories {_ns.count(), _ns.count()};

        for (int f {0}; f < _ns.count(); ++f)
        {
          assert (TAKEN (0, f) || TAKEN (1, f));

          if (TAKEN (0, f) && !memories[0].seen (f))
          {
            sizes[0].emplace_back (size_of_component (memories[0], 0, f));
          }
          else if (TAKEN (1, f) && !memories[1].seen (f))
          {
            sizes[1].emplace_back (size_of_component (memories[1], 1, f));
          }
        }

        std::sort (sizes[0].begin(), sizes[0].end(), std::greater<int>());
        std::sort (sizes[1].begin(), sizes[1].end(), std::greater<int>());

        decltype (sizes[0].size()) p {0};

        while (p < sizes[0].size() && p < sizes[1].size())
        {
          if (sizes[0][p] > sizes[1][p])
          {
            return 0;
          }
          if (sizes[0][p] < sizes[1][p])
          {
            return 1;
          }
          ++p;
        }

        if (p == sizes[0].size())
        {
          assert (p < sizes[1].size());

          return 1;
        }

        assert (p == sizes[1].size());
        assert (p < sizes[0].size());

        return 0;
      }

      assert (_available > 0);

      std::uint64_t key {_val[0]};
      key <<= 32;
      key += _val[1];

      {
        auto cached {_storage.get (key, _available, _player)};
        ++count_gets;

        if (cached != 0b11)
        {
          ++count_hits;

          return cached == 0b10 ? 0 : 1;
        }
      }

      int const available (_available);
      int const score (_score[_player]);

      switch (_available)
      {
      case 3:
        for (int x {0}; x + 2 < _ns.count(); ++x)
        {
          if (EMPTY (x))
          {
            for (int y {x + 1}; y + 1 < _ns.count(); ++y)
            {
              if (EMPTY (y))
              {
                for (int z {y + 1}; z < _ns.count(); ++z)
                {
                  if (EMPTY (z))
                  {
                    _val[_player] |= (1 << x);
                    _val[_player] |= (1 << y);
                    _val[_player] |= (1 << z);
                    _free -= 3;
                    memory m {_ns.count()};
                    int const c (std::max ( size_of_component (m, _player, x)
                                ,std::max ( size_of_component (m, _player, y)
                                          , size_of_component (m, _player, z)
                                          )
                                          )
                                );
                    if (c > _score[_player])
                    {
                      _score[_player] = c;
                      _available =
                        std::min (_free, (c > 1 && c >= _score[1 - _player]) ? 3 : 2);
                    }
                    _player = 1 - _player;
                    int const child (winner());
                    _player = 1 - _player;
                    _score[_player] = score;
                    _available = available;
                    _free += 3;
                    _val[_player] &= ~(1 << x);
                    _val[_player] &= ~(1 << y);
                    _val[_player] &= ~(1 << z);
                    if (child == _player)
                    {
                      _storage.put (key, _available, _player, _player);

                      return _player;
                    }
                  }
                }
              }
            }
          }
        }
        [[fallthrough]];
      case 2:
        for (int x {0}; x + 1 < _ns.count(); ++x)
        {
          if (EMPTY (x))
          {
            for (int y {x + 1}; y < _ns.count(); ++y)
            {
              if (EMPTY (y))
              {
                _val[_player] |= (1 << x);
                _val[_player] |= (1 << y);
                _free -= 2;
                memory m {_ns.count()};
                int const c (std::max ( size_of_component (m, _player, x)
                                      , size_of_component (m, _player, y)
                                      )
                            );
                if (c > _score[_player])
                {
                  _score[_player] = c;
                  _available =
                    std::min (_free, (c > 1 && c >= _score[1 - _player]) ? 3 : 2);
                }
                _player = 1 - _player;
                int const child (winner());
                _player = 1 - _player;
                _score[_player] = score;
                _available = available;
                _free += 2;
                _val[_player] &= ~(1 << x);
                _val[_player] &= ~(1 << y);
                if (child == _player)
                {
                  _storage.put (key, _available, _player, _player);

                  return _player;
                }
              }
            }
          }
        }
        [[fallthrough]];
      case 1:
        for (int x {0}; x < _ns.count(); ++x)
        {
          if (EMPTY (x))
          {
            _val[_player] |= (1 << x);
            _free -= 1;
            memory m {_ns.count()};
            int const c (size_of_component (m, _player, x));
            if (c > _score[_player])
            {
              _score[_player] = c;
              _available =
                std::min (_free, (c > 1 && c >= _score[1 - _player]) ? 3 : 2);
            }
            _player = 1 - _player;
            int const child (winner());
            _player = 1 - _player;
            _score[_player] = score;
            _available = available;
            _free += 1;
            _val[_player] &= ~(1 << x);
            if (child == _player)
            {
              _storage.put (key, _available, _player, _player);

              return _player;
            }
          }
        }
        _storage.put (key, _available, _player, 1 - _player);
        return 1 - _player;

      default:
        abort();
      }
    }
  private:
    neighbours _ns;
    std::array<int, 2> _score;
    int _player;
    int _available;
    int _free;
    std::array<std::uint32_t, 2> _val;
    storage& _storage;
    mutable std::vector<int> _open;
  };
}

int main()
{
  int n {0};
  int s {0};
  auto shift ([&s] (int x) -> int { return s + x; });
  std::string line;

  std::cout << "graph\n{\n";

  while (std::getline (std::cin, line))
  {
    neighbours const neighbours {line.data()};
    storage storage;

    for (int f {0}; f < neighbours.count(); ++f)
    {
      std::cout << shift (f)
                << " [shape=\"circle\",height=0.25,label=\""
        //                << (f == 0 ? std::to_string (s) : "")
                << std::to_string (shift (f))
                << "\""
                << ( board {neighbours, f, storage}.winner() == 0
                   ? ",style=\"filled\""
                   : ""
                   )
                << "];\n"
        ;
    }

    for (int f {0}; f < neighbours.count(); ++f)
    {
      for (int k {0}; k < neighbours.count (f); ++k)
      {
        if (f < neighbours (f, k))
        {
          std::cout << shift (f) << "--" << shift (neighbours (f, k)) << ";";
        }
      }
    }

    std::cout << '\n';

    s += neighbours.count();
    ++n;
  }

  std::cout << "}\n";

  std::cerr << "calls_to_winner " << count_calls_to_winner << std::endl;
  std::cerr << "puts " << count_puts
            << " gets " << count_gets
            << " hits " << count_hits
            << " score_other " << count_score_other
            << " score_player " << count_score_player
            << " leafs " << count_leafs
            << std::endl;

  return 0;
}
