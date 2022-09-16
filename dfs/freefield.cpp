#include <array>
#include <concepts>
#include <cstdint>
#include <functional>
#include <iterator>

namespace
{
  template<std::unsigned_integral Field, std::size_t Size>
    struct FreeField
  {
    template<std::size_t N> using Fields = std::array<Field, N>;

    constexpr explicit FreeField() noexcept;

    constexpr auto size() const noexcept -> std::size_t;
    constexpr auto put (Field) noexcept -> void;
    constexpr auto unput() noexcept -> Field;

    struct iterator
    {
      using value_type = Field;
      using difference_type = std::ptrdiff_t;
      using reference = value_type const&;
      using pointer = value_type const*;
      using iterator_category = std::input_iterator_tag;

      constexpr auto operator++() noexcept -> iterator&;
      constexpr auto operator++(int) noexcept -> iterator&;
      constexpr auto operator*() const noexcept -> reference;
      constexpr auto operator->() const noexcept -> pointer;

      constexpr auto operator== (iterator const& other) const noexcept -> bool;
      constexpr auto operator!= (iterator const& other) const noexcept -> bool;

    private:
      friend struct FreeField<Field, Size>;
      constexpr iterator
        ( std::reference_wrapper<Fields<Size + 1> const>
        , std::size_t
        ) noexcept;

      std::reference_wrapper<Fields<Size + 1> const> _next;
      std::size_t _size;
      Field _f;
    };
    static_assert (std::is_copy_constructible_v<iterator>);
    static_assert (std::is_copy_assignable_v<iterator>);
    static_assert (std::is_destructible_v<iterator>);
    static_assert (std::is_swappable_v<iterator>);
    static_assert (std::equality_comparable<iterator>);

    constexpr auto begin() const noexcept -> iterator;
    constexpr auto end() const noexcept-> iterator;

  private:
    Fields<Size> _put;
    Fields<Size + 1> _next;
    Fields<Size + 1> _prev;
    Field _used {0};
  };
}

#include <cassert>
#include <memory>

namespace
{
  template<std::unsigned_integral Field, std::size_t Size>
    constexpr FreeField<Field, Size>::FreeField() noexcept
  {
    _next.fill (1);
    _prev.fill (1);
  }

  template<std::unsigned_integral Field, std::size_t Size>
    constexpr std::size_t FreeField<Field, Size>::size() const noexcept
  {
    return Size - _used;
  }

  template<std::unsigned_integral Field, std::size_t Size>
    constexpr auto FreeField<Field, Size>::put (Field f) noexcept -> void
  {
    assert (_used < Size);
    assert (f < Size);

    auto const p {_prev[f + 0]};
    auto const n {_next[f + 1]};

    assert (p > 0);
    assert (f + 1 >= p);
    assert (n > 0);
    assert (f + n <= Size);

    _next[f + 1 - p] += n;
    _prev[f + 0 + n] += p;

    _put[_used++] = f;
  }

  template<std::unsigned_integral Field, std::size_t Size>
    constexpr auto FreeField<Field, Size>::unput() noexcept -> Field
  {
    assert (_used > 0);
    assert (_used <= Size);

    auto const f {_put[--_used]};

    assert (f < Size);

    auto const p {_prev[f + 0]};
    auto const n {_next[f + 1]};

    assert (p > 0);
    assert (f + 1 >= p);
    assert (n > 0);
    assert (f + n <= Size);

    _next[f + 1 - p] -= n;
    _prev[f + 0 + n] -= p;

    return f;
  }

  template<std::unsigned_integral Field, std::size_t Size>
    constexpr auto FreeField<Field, Size>::iterator::operator++() noexcept
      -> iterator&
  {
    _f += _next.get()[_f + 1];

    --_size;

    return *this;
  }
  template<std::unsigned_integral Field, std::size_t Size>
    constexpr auto FreeField<Field, Size>::iterator::operator++(int) noexcept
      -> iterator&
  {
   decltype (*this) old {*this};

    --*this;

    return old;
  }

  template<std::unsigned_integral Field, std::size_t Size>
    constexpr auto FreeField<Field, Size>::iterator::operator==
      ( iterator const& other
      ) const noexcept -> bool
  {
    return _size == other._size;
  }
  template<std::unsigned_integral Field, std::size_t Size>
    constexpr auto FreeField<Field, Size>::iterator::operator!=
      ( iterator const& other
      ) const noexcept -> bool
  {
    return _size != other._size;
  }

  template<std::unsigned_integral Field, std::size_t Size>
    constexpr auto FreeField<Field, Size>::iterator::operator*() const noexcept
      -> reference
  {
    return _f;
  }
  template<std::unsigned_integral Field, std::size_t Size>
    constexpr auto FreeField<Field, Size>::iterator::operator->() const noexcept
      -> pointer
  {
    return std::addressof (_f);
  }

  template<std::unsigned_integral Field, std::size_t Size>
    constexpr FreeField<Field, Size>::iterator::iterator
      ( std::reference_wrapper<Fields<Size + 1> const> next
      , std::size_t size
      ) noexcept
        : _next {next}
        , _size {size}
        , _f {_next.get()[0]}
  {
    assert (_f > 0);

    --_f;
  }

  template<std::unsigned_integral Field, std::size_t Size>
    constexpr auto FreeField<Field, Size>::begin() const noexcept -> iterator
  {
    return {_next, size()};
  }

  template<std::unsigned_integral Field, std::size_t Size>
    constexpr auto FreeField<Field, Size>::end() const noexcept -> iterator
  {
    return {_next, 0};
  }
}

#include <iostream>

namespace
{
  template<std::unsigned_integral Field, std::size_t Size>
    struct print
  {
    print (FreeField<Field, Size> const& ff)
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
    FreeField<Field, Size> const& _ff;
  };
  template<std::unsigned_integral Field, std::size_t Size>
    std::ostream& operator<< (std::ostream& os, print<Field, Size> const& p)
  {
    return p (os);
  }
}

int main()
{
  using field = unsigned short;
  std::size_t const size (7);

  FreeField<field, size> ff;

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
