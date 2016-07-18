#include <memory>
#include <type_traits>

namespace
{
  template<typename Field, std::size_t Size>
    struct freefield
  {
    freefield (freefield const&) = delete;
    freefield (freefield&&) = delete;
    freefield& operator= (freefield const&) = delete;
    freefield& operator= (freefield&&) = delete;
    ~freefield() = default;

    using field_type
      = typename std::enable_if<std::is_unsigned<Field>::value, Field>::type;

    freefield();

    std::size_t size() const;
    void put (field_type);
    void unput();

    struct iterator
    {
      void operator++();
      bool operator!= (iterator const&);
      field_type operator*() const;

    private:
      friend struct freefield<Field, Size>;
      iterator (field_type const*, std::size_t);

      field_type const* _next;
      field_type _f;
      std::size_t _size;
    };
    iterator begin() const;
    iterator end() const;

  private:
    std::unique_ptr<field_type> _dat;
    field_type* _put;
    field_type* _next;
    field_type* _prev;
    std::size_t _used;
  };
}

#include <algorithm>
#include <cassert>

namespace
{
  // |---------------||---------------|--------------||
  // ^               ^^               ^              :^
  // 0               :Size + 1        2 * Size + 1   :3 * Size + 2
  // :               ::               :              ::
  // |...............):               :              ::  put
  //                 #|---------------)              ::  first + next
  //                                  |--------------)#  prev + last

  template<typename Field, std::size_t Size>
    freefield<Field, Size>::freefield()
      : _dat (new field_type[3 * Size + 2])
      , _put (_dat.get())
      , _next (_dat.get() + Size + 1)
      , _prev (_dat.get() + 2 * Size + 1)
      , _used (0)
  {
    std::fill (_dat.get() + Size, _dat.get() + 3 * Size + 2, 1);
  }

  template<typename Field, std::size_t Size>
    std::size_t freefield<Field, Size>::size() const
  {
    return Size - _used;
  }

  template<typename Field, std::size_t Size>
    void freefield<Field, Size>::put (field_type f)
  {
    assert (f < Size);

    _next[f - _prev[f]] += _next[f];
    _prev[f + _next[f]] += _prev[f];

    _put[_used++] = f;
  }

  template<typename Field, std::size_t Size>
    void freefield<Field, Size>::unput()
  {
    assert (_used > 0);

    field_type const f (_put[--_used]);

    _next[f - _prev[f]] -= _next[f];
    _prev[f + _next[f]] -= _prev[f];
  }

  template<typename Field, std::size_t Size>
    void freefield<Field, Size>::iterator::operator++()
  {
    _f += _next[_f];
    --_size;
  }

  template<typename Field, std::size_t Size>
    bool freefield<Field, Size>::iterator::operator!= (iterator const& other)
  {
    return _size != other._size;
  }

  template<typename Field, std::size_t Size>
    typename freefield<Field, Size>::field_type
      freefield<Field, Size>::iterator::operator*() const
  {
    return _f;
  }

  template<typename Field, std::size_t Size>
    freefield<Field, Size>::iterator::iterator ( field_type const* next
                                               , std::size_t size
                                               )
      : _next (next)
      , _f (*(next - 1) - 1)
      , _size (size)
  {}

  template<typename Field, std::size_t Size>
    typename freefield<Field, Size>::iterator
      freefield<Field, Size>::begin() const
  {
    return {_next, size()};
  }

  template<typename Field, std::size_t Size>
    typename freefield<Field, Size>::iterator
      freefield<Field, Size>::end() const
  {
    return {_next, 0};
  }
}

#include <iostream>

namespace
{
  template<typename Field, std::size_t Size>
    struct print
  {
    print (freefield<Field, Size> const& ff)
      : _ff (ff)
    {}
    std::ostream& operator() (std::ostream& os) const
    {
      os << _ff.size() << ':';

      for (auto const& f : _ff)
      {
        os << ' ' << f;
      }

      return os;
    }
    freefield<Field, Size> const& _ff;
  };
  template<typename Field, std::size_t Size>
    std::ostream& operator<< (std::ostream& os, print<Field, Size> const& p)
  {
    return p (os);
  }
}

int main()
{
  using field = unsigned short;
  std::size_t const size (7);

  freefield<field, size> ff;

  std::cout << print<field, size> (ff) << '\n';

  ff.put (4);

  std::cout << print<field, size> (ff) << '\n';

  ff.put (1);

  std::cout << print<field, size> (ff) << '\n';

  ff.unput();

  std::cout << print<field, size> (ff) << '\n';

  ff.unput();

  std::cout << print<field, size> (ff) << '\n';

  ff.put (4);
  ff.put (2);
  ff.put (1);
  ff.put (0);
  ff.put (5);
  ff.put (6);
  ff.put (3);

  std::cout << print<field, size> (ff) << '\n';

  ff.unput();
  ff.unput();
  ff.unput();
  ff.unput();
  ff.unput();
  ff.unput();
  ff.unput();

  std::cout << print<field, size> (ff) << '\n';

  std::cout << sizeof (ff) << '\n';

  return 0;
}
