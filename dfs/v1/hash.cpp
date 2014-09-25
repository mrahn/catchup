// mirko.jesiak@web.de

#include <hash.hpp>

#include <iostream>

int main()
{
  constexpr unsigned SIZE {2};

  table<uint64_t, uint64_t, bucket<uint64_t, SIZE>, hash_int> h
    (1UL << 31);

  std::cout << "size " << h.size()
            << " buckets " << h.capacity()
            << " of capacity " << bucket<uint64_t, SIZE>::capacity()
            << " = " << (h.capacity() * bucket<uint64_t, SIZE>::capacity())
            << std::endl;

  for (uint64_t i (0); i < 1UL << 26; ++i)
  {
    h.put (i, i);
  }

  std::size_t elem (0);

  for (uint64_t i (0); i < 1UL << 26; ++i)
  {
    elem += h.has (i, i) ? 1 : 0;
  }

  std::cout << "has " << elem << std::endl;

  return 0;
}
