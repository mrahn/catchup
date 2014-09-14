// mirko.jesiak@web.de

#ifndef CATCHUP_HASH_HPP
#define CATCHUP_HASH_HPP

#include <algorithm>
#include <cstdint>
#include <vector>
#include <iostream>

namespace
{
  template<typename V, unsigned SIZE = 5>
  class bucket
  {
  public:
    bucket()
      : _value()
      , _pos (0)
    {}
    void add (V value)
    {
      _value[(_pos++) % SIZE] = value;
    }
    bool has (V value) const
    {
      return std::find (_value, _value + SIZE, value) != _value + SIZE;
    }
    static std::size_t capacity()
    {
      return SIZE;
    }

  private:
    V _value[SIZE];
    unsigned _pos;
  };

  template<typename V>
  class bucket<V, 0U>
  {
  public:
    bucket() {};
    void add (V) const {};
    bool has (V) const { return false; }
    static std::size_t capacity() { return 0; }
  };

  template<typename V>
  class bucket<V, 1U>
  {
  public:
    bucket() : _value() {}
    void add (V value) { _value = value; }
    bool has (V value) const { return _value == value; }
    static std::size_t capacity() { return 1; }

  private:
    V _value;
  };

  template<typename K, typename V, typename Bucket, uint64_t (*Hash) (K)>
  class table
  {
  public:
    table (std::size_t size)
      : _size (size)
      , _capacity (_size / sizeof (std::pair<bool, Bucket>))
      , _xs (_capacity)

      , _num_put (0)
      , _num_hit (0)
      , _num_miss (0)
    {
      std::cerr << "cache: size " << _size
                << " capacity " << _capacity
                << std::endl;
    }
    ~table()
    {
      std::cerr << "cache: put " << _num_put
                << " hit " << _num_hit
                << " miss " << _num_miss
                << std::endl;
    }

    void put (K k, V v)
    {
      std::pair<bool, Bucket>& entry (_xs[Hash (k) % _capacity]);

      entry.first = true;
      entry.second.add (v);

      ++_num_put;
    }
    bool has (K k, V v) const
    {
      std::pair<bool, Bucket> const& entry (_xs[Hash (k) % _capacity]);

      bool const found (entry.first && entry.second.has (v));

      if (found)
      {
        ++_num_hit;
      }
      else
      {
        ++_num_miss;
      }

      return found;
    }

    std::size_t num_put() const
    {
      return _num_put;
    }
    std::size_t num_hit() const
    {
      return _num_hit;
    }
    std::size_t num_miss() const
    {
      return _num_miss;
    }
    std::size_t capacity() const
    {
      return _capacity;
    }
    std::size_t size() const
    {
      return _size;
    }

  private:
    std::size_t const _size;
    std::size_t const _capacity;
    std::vector<std::pair<bool, Bucket>> _xs;

    std::size_t mutable _num_put;
    std::size_t mutable _num_hit;
    std::size_t mutable _num_miss;
  };
}

namespace
{
  // MurmurHash3
  uint64_t hash_int (uint64_t k)
  {
    k ^= k >> 33;
    k *= 0xff51afd7ed558ccd;
    k ^= k >> 33;
    k *= 0xc4ceb9fe1a85ec53;
    k ^= k >> 33;

    return k;
  }
}

#endif
