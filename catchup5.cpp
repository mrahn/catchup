// mirko.jesiak@web.de

#include <algorithm>
#include <cstdint>
#include <iostream>

namespace
{
  namespace player
  {
    typedef short player;

#define BLUE 0
#define ORANGE 1
#define NONE 2

    class show
    {
    public:
      show (player player)
        : _player (player)
      {}
      std::ostream& operator() (std::ostream& os) const
      {
        return os << (_player == BLUE ? 'B' : _player == ORANGE ? 'O' : '.');
      }
    private:
      player _player;
    };
    std::ostream& operator<< (std::ostream& os, show const& s)
    {
      return s (os);
    }
  }

  class hash
  {
    struct bucket
    {
      bucket()
      {
        std::fill (_value, _value + 2, 0);
      };
      uint64_t _value[2];
      player::player _winner[6]; // B1 B2 B3 O1 O2 O3
    };

  public:
    hash (std::size_t size)
      : _size (size)
      , _capacity (size / sizeof (bucket))
      , _buckets (_capacity)
      , _put_board (0)
      , _replace_board (0)
      , _put_position (0)
      , _replace_position (0)
      , _hit (0)
      , _miss_board (0)
      , _miss_position (0)
    {
      std::cout << "cache: size " << _size
                << " boards " << _capacity
                << " positions " << _capacity * 6
                << std::endl;
    }
    ~hash()
    {
      std::size_t num_board (0);
      std::size_t num_position (0);

      for (bucket& b : _buckets)
      {
        if (b._value[0] != 0 || b._value[1] != 0)
        {
          ++num_board;

          for (player::player result (0); result < 6; ++result)
          {
            if (b._winner[result] != NONE)
            {
              ++num_position;
            }
          }
        }
      }

      std::cout << "cache: put_board " << _put_board
                << " replace_board " << _replace_board
                << " put_position " << _put_position
                << " replace_position " << _replace_position
                << " num_board " << num_board
                << " num_position " << num_position
                << " hit " << _hit
                << " miss_board " << _miss_board
                << " miss_position " << _miss_position
                << std::endl;
    }

    void put ( uint64_t key
             , uint64_t value[2]
             , player::player to_move
             , short available_stones
             , player::player winner
             )
    {
      bucket& b (_buckets[key % _capacity]);

      if (b._value[0] != value[0] || b._value[1] != value[1])
      {
        if (b._value[0] != 0 || b._value[1] != 0)
        {
          ++_replace_board;

          for (player::player result (0); result < 6; ++result)
          {
            if (b._winner[result] != NONE)
            {
              ++_replace_position;
            }
          }
        }

        std::copy (value, value + 2, b._value);
        std::fill (b._winner, b._winner + 6, NONE);
        ++_put_board;
      }

      switch (available_stones)
      {
      case 1:  b._winner[3 * to_move + 3 - 1] = winner;
      case 2:  b._winner[3 * to_move + 3 - 2] = winner;
      default: b._winner[3 * to_move + 3 - 3] = winner;
      }

      ++_put_position;
    }

    player::player get ( uint64_t key
                       , uint64_t value[2]
                       , player::player to_move
                       , short available_stones
                       ) const
    {
      bucket const& b (_buckets[key % _capacity]);

      if (b._value[0] != value[0] || b._value[1] != value[1])
      {
        ++_miss_board;

        return NONE;
      }

      player::player const result
        (b._winner[3 * to_move + 3 - available_stones]);

      if (result != NONE)
      {
        ++_hit;
      }
      else
      {
        ++_miss_position;
      }

      return result;
    }

  private:
    std::size_t _size;
    std::size_t _capacity;
    std::vector<bucket> _buckets;
    mutable std::size_t _put_board;
    mutable std::size_t _replace_board;
    mutable std::size_t _put_position;
    mutable std::size_t _replace_position;
    mutable std::size_t _hit;
    mutable std::size_t _miss_board;
    mutable std::size_t _miss_position;
  };

  class show;

  class board
  {
  public:
    board (board const&) = delete;
    board operator= (board const&) = delete;
    board (board&&) = delete;
    board operator= (board&&) = delete;

    board (std::size_t cache_size_in_bytes)
      : _cache (cache_size_in_bytes)
      , _to_move (BLUE)
      , _taken()
      , _high_water()
      , _free_fields (61)
      , _available_stones (1)
      , _seen()
      , _leaf (0)
      , _high_water_other (0)
      , _high_water_to_move (0)
    {
      std::fill (_taken, _taken + 61, NONE);
      std::fill (_high_water, _high_water + 2, 0);
    };
    ~board()
    {
      std::cout << "board: leaf " << _leaf
                << " high_water_other " << _high_water_other
                << " high_water_to_move " << _high_water_to_move
                << std::endl;
    }

    void put (short);
    void put (short, short);
    void put (short, short, short);

    std::pair<uint64_t, player::player> size_of_tree_for_winning_move()
    {
      return size_of_tree_for_winning_move (0);
    }

  private:
    uint64_t _magic[61 * 3] = {16438978298268261614UL,11882579674973188460UL,924148630519662205UL,578567204279203355UL,14712694358954801360UL,13361980795859999429UL,5194443201411278601UL,13686422300643180717UL,11395962869650399376UL,5994050210098849117UL,14572738088570370502UL,16002389424380271896UL,8933160535496144779UL,6170599787934868873UL,17878650006760841585UL,9411053256018458925UL,14233600796685358787UL,3252389778782514935UL,6707424278561455747UL,2444956123759445708UL,10946964618991686719UL,7565577008557633986UL,3188095973916356822UL,6268333515667327818UL,7753532560949987642UL,12624850232354291418UL,12383819754456653311UL,7321861418321890364UL,9400270237303778169UL,17175506669587057225UL,2213781315918693126UL,10604228656201361559UL,13764727328199855917UL,13495346504220335073UL,8223111015344448497UL,1483753006360394210UL,3044917804316769253UL,10890314317703366631UL,7995376020854169597UL,11630667627591396655UL,15481450477766378329UL,15374572320027978377UL,11736615485081388760UL,8017526455688374097UL,1467073424441746775UL,11334748333642515120UL,7822230788301418788UL,7058831600524178522UL,4370078580612483493UL,7013891191106577733UL,12670653662399705628UL,2770318086406537615UL,3919518369811656587UL,6855323779976241372UL,6550033966295872134UL,3404838698710669359UL,15130399219270990918UL,17011807392380606027UL,14483795061144979UL,3818565583893497252UL,10486996387045218219UL,11485054189629491364UL,954435750495184955UL,6056168405895771837UL,13604061963473978818UL,13740807106444456862UL,3615749362799659180UL,12146897367291631124UL,12547284284979126591UL,12047971629377209016UL,1433443230573967559UL,17074491683082448381UL,11278975603778226277UL,15185921780160227880UL,621870476298190601UL,17078005624254982021UL,13092998196255926090UL,2141057706505310293UL,6810135280406839378UL,7498538466271642179UL,2786465847161338551UL,8707684501959772902UL,4923568992912560219UL,18271024699482214708UL,3516803542008022876UL,1104477561862641282UL,8577904220044442713UL,15549079161419420844UL,9423092205017401748UL,15560672003307604447UL,7679990969239491234UL,18008702148336706539UL,16089448614733742112UL,12430975306498513400UL,11345594493500804814UL,3170965237682408989UL,3916579126892118757UL,12777887110676000335UL,4219547184991366949UL,290891913940626891UL,10527740316125698616UL,15514467526672129239UL,3110671411459920834UL,5278989475561548693UL,16561265626771212023UL,367883596375528408UL,16650364482347994065UL,12800153707643806989UL,5148376365050556232UL,7866107008330680186UL,12185995218792759378UL,15101483593097170082UL,12030315200963316041UL,13850331654991889674UL,11342217530930475291UL,8432368565120606600UL,11834350433856491077UL,9864086654735727697UL,4807705961004826636UL,17689302747270588808UL,6545382567592668809UL,2796847707156128995UL,14748207860872356330UL,12332012325581048001UL,17468575921864456485UL,6669726554997484221UL,862151181287963059UL,8574960223287001237UL,18378247186412969330UL,2430828540395962783UL,6707223314552716538UL,7526745505754377043UL,3726309053229514341UL,4410878063922818885UL,10331110272522807600UL,12234030055190273115UL,11390621275807863422UL,1539792900591489186UL,9700366050374390766UL,9544588192884091744UL,6096195415763550941UL,641686637039511459UL,7546842785630248858UL,1855035216917874105UL,226711632731057320UL,6212200614659095303UL,15411860853648293064UL,17631700086396951507UL,3262948334320699006UL,7133836989455035567UL,7730977119927390392UL,3807172849157586272UL,11034052366445826545UL,13484634813047280743UL,7438092001298116200UL,9043166540803695514UL,9389425931436194397UL,11469938571565920821UL,17370298838561858599UL,4830282877255227953UL,5075017110956724003UL,15552706711422061195UL,9316940989070949348UL,15728005895442724650UL,10787576847717388827UL,10272635148677219029UL,16199574881732564331UL,7159032472519886904UL,15580781750273713121UL,3214265352320831182UL,7305000960707596693UL,10403264289315477750UL,17451367988431409674UL,2101094149623178370UL,9427408202659726720UL,17378728192318928827UL,11409381981722942431UL,16622504632581675184UL,3171465991678580822UL,5399349150747559200UL,14984250639972372429UL,3015617975943369676UL,11305467432302751004UL};

    std::pair<uint64_t, player::player> size_of_tree_for_winning_move (short);

    void unput (short, short high_water, short available_stones);
    void unput (short, short, short high_water, short available_stones);
    void unput (short, short, short, short high_water, short available_stones);

    friend class show;

    short neighbour_begin[62] = {0,3,7,11,15,18,22,28,34,40,46,50,54,60,66,72,78,84,88,92,98,104,110,116,122,128,132,135,141,147,153,159,165,171,177,180,184,190,196,202,208,214,220,224,228,234,240,246,252,258,262,266,272,278,284,290,294,297,301,305,309,312};
    short neighbour[312] = {1,5,6,0,2,6,7,1,3,7,8,2,4,8,9,3,9,10,0,6,11,12,0,1,5,7,12,13, 1,2,6,8,13,14,2,3,7,9,14,15,3,4,8,10,15,16,4,9,16,17,5,12,18,19,5,6,11,13,19,20,6,7,12,14,20,21,7,8,13,15,21,22,8,9,14,16,22,23,9,10,15,17,23,24,10,16,24,25,11,19,26,27,11,12,18,20,27,28,12,13,19,21,28,29,13,14,20,22,29,30,14,15,21,23,30,31,15,16,22,24,31,32,16,17,23,25,32,33,17,24,33,34,18,27,35,18,19,26,28,35,36,19,20,27,29,36,37,20,21,28,30,37,38,21,22,29,31,38,39,22,23,30,32,39,40,23,24,31,33,40,41,24,25,32,34,41,42,25,33,42,26,27,36,43,27,28,35,37,43,44,28,29,36,38,44,45,29,30,37,39,45,46,30,31,38,40,46,47,31,32,39,41,47,48,32,33,40,42,48,49,33,34,41,49,35,36,44,50,36,37,43,45,50,51,37,38,44,46,51,52,38,39,45,47,52,53,39,40,46,48,53,54,40,41,47,49,54,55,41,42,48,55,43,44,51,56,44,45,50,52,56,57,45,46,51,53,57,58,46,47,52,54,58,59,47,48,53,55,59,60,48,49,54,60,50,51,57,51,52,56,58,52,53,57,59,53,54,58,60,54,55,59};
    short translation[671] = {4,3,2,1,0,10,9,8,7,6,5,17,16,15,14,13,12,11,25,24,23,22,21,20,19,18,34,33,32,31,30,29,28,27,26,42,41,40,39,38,37,36,35,49,48,47,46,45,44,43,55,54,53,52,51,50,60,59,58,57,56,26,35,43,50,56,18,27,36,44,51,57,11,19,28,37,45,52,58,5,12,20,29,38,46,53,59,0,6,13,21,30,39,47,54,60,1,7,14,22,31,40,48,55,2,8,15,23,32,41,49,3,9,16,24,33,42,4,10,17,25,34,34,42,49,55,60,25,33,41,48,54,59,17,24,32,40,47,53,58,10,16,23,31,39,46,52,57,4,9,15,22,30,38,45,51,56,3,8,14,21,29,37,44,50,2,7,13,20,28,36,43,1,6,12,19,27,35,0,5,11,18,26,56,50,43,35,26,57,51,44,36,27,18,58,52,45,37,28,19,11,59,53,46,38,29,20,12,5,60,54,47,39,30,21,13,6,0,55,48,40,31,22,14,7,1,49,41,32,23,15,8,2,42,33,24,16,9,3,34,25,17,10,4,60,55,49,42,34,59,54,48,41,33,25,58,53,47,40,32,24,17,57,52,46,39,31,23,16,10,56,51,45,38,30,22,15,9,4,50,44,37,29,21,14,8,3,43,36,28,20,13,7,2,35,27,19,12,6,1,26,18,11,5,0,26,18,11,5,0,35,27,19,12,6,1,43,36,28,20,13,7,2,50,44,37,29,21,14,8,3,56,51,45,38,30,22,15,9,4,57,52,46,39,31,23,16,10,58,53,47,40,32,24,17,59,54,48,41,33,25,60,55,49,42,34,34,25,17,10,4,42,33,24,16,9,3,49,41,32,23,15,8,2,55,48,40,31,22,14,7,1,60,54,47,39,30,21,13,6,0,59,53,46,38,29,20,12,5,58,52,45,37,28,19,11,57,51,44,36,27,18,56,50,43,35,26,0,5,11,18,26,1,6,12,19,27,35,2,7,13,20,28,36,43,3,8,14,21,29,37,44,50,4,9,15,22,30,38,45,51,56,10,16,23,31,39,46,52,57,17,24,32,40,47,53,58,25,33,41,48,54,59,34,42,49,55,60,4,10,17,25,34,3,9,16,24,33,42,2,8,15,23,32,41,49,1,7,14,22,31,40,48,55,0,6,13,21,30,39,47,54,60,5,12,20,29,38,46,53,59,11,19,28,37,45,52,58,18,27,36,44,51,57,26,35,43,50,56,60,59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0,56,57,58,59,60,50,51,52,53,54,55,43,44,45,46,47,48,49,35,36,37,38,39,40,41,42,26,27,28,29,30,31,32,33,34,18,19,20,21,22,23,24,25,11,12,13,14,15,16,17,5,6,7,8,9,10,0,1,2,3,4};

    hash _cache;
    player::player _to_move;
    player::player _taken[61];
    short _high_water[2];
    short _free_fields;
    short _available_stones;

    bool _seen[61];
    short size_of_component (short f, player::player);

    player::player in_front();

    mutable std::size_t _leaf;
    mutable std::size_t _high_water_other;
    mutable std::size_t _high_water_to_move;
  };

#define UNSEE_ALL() std::fill (_seen, _seen + 61, false);
#define SEEN(field) _seen[field]
#define SEE(field) _seen[field] = true

#define TAKE(f) _taken[f] = _to_move; --_free_fields
#define UNTAKE(f) _taken[f] = NONE; ++_free_fields
#define IS_TAKEN(p, f) (_taken[f] == p)
#define IS_FREE(f) (_taken[f] == NONE)

  class show
  {
  public:
    show (board const& board)
      : _board (board)
    {}
    std::ostream& operator() (std::ostream& os) const
    {
      os << "to_move " << player::show (_board._to_move)
         << " high_water " << _board._high_water[BLUE]
         << " " << _board._high_water[ORANGE]
         << " available_stones " << _board._available_stones
         << " free_fields " << _board._free_fields
         << '\n';

      auto const line
        ( [this, &os] (short begin, short end)
          {
            for (short field (begin); field < end; ++field)
            {
              os << ' ' << player::show (_board._taken[field]);
            }
            os << '\n';
          }
        );

      os << "    "; line (0, 5);
      os << "   "; line (5, 11);
      os << "  "; line (11, 18);
      os << " "; line (18, 26);
      os << ""; line (26, 35);
      os << " "; line (35, 43);
      os << "  "; line (43, 50);
      os << "   "; line (50, 56);
      os << "    "; line (56, 61);

      return os;
    }
  private:
    board const& _board;
  };
  std::ostream& operator<< (std::ostream& os, show const& s)
  {
    return s (os);
  }

#define SWITCH_PLAYER() _to_move = 1 - _to_move

#define INIT_TRAVERSALS()                               \
  const short high_water_old (_high_water[_to_move]);   \
                                                        \
  UNSEE_ALL()

#define TRAVERSE(f)                                                     \
  _high_water[_to_move] = std::max ( _high_water[_to_move]              \
                                   , size_of_component (f, _to_move)    \
                                   )

#define FINALIZE_TRAVERSALS()                                           \
  _available_stones =                                                   \
    (  _high_water[_to_move] > high_water_old                           \
    && _high_water[_to_move] > _high_water[1 - _to_move]                \
    && _high_water[_to_move] > 1                                        \
    ) ? 3 : 2

  void board::put (short f)
  {
    TAKE (f);

    INIT_TRAVERSALS();

    TRAVERSE (f);

    FINALIZE_TRAVERSALS();

    SWITCH_PLAYER();
  }
  void board::put (short f, short g)
  {
    TAKE (f);
    TAKE (g);

    INIT_TRAVERSALS();

    TRAVERSE (f);
    TRAVERSE (g);

    FINALIZE_TRAVERSALS();

    SWITCH_PLAYER();
  }
  void board::put (short f, short g, short h)
  {
    TAKE (f);
    TAKE (g);
    TAKE (h);

    INIT_TRAVERSALS();

    TRAVERSE (f);
    TRAVERSE (g);
    TRAVERSE (h);

    FINALIZE_TRAVERSALS();

    SWITCH_PLAYER();
  }

#undef FINALIZE_TRAVERSALS
#undef TRAVERSE
#undef INIT_TRAVERSALS

  void board::unput (short f, short high_water, short available_stones)
  {
    SWITCH_PLAYER();

    _high_water[_to_move] = high_water;
    _available_stones = available_stones;

    UNTAKE (f);
  }
  void board::unput (short f, short g, short high_water, short available_stones)
  {
    SWITCH_PLAYER();

    _high_water[_to_move] = high_water;
    _available_stones = available_stones;

    UNTAKE (f);
    UNTAKE (g);
  }
  void board::unput
    (short f, short g, short h, short high_water, short available_stones)
  {
    SWITCH_PLAYER();

    _high_water[_to_move] = high_water;
    _available_stones = available_stones;

    UNTAKE (f);
    UNTAKE (g);
    UNTAKE (h);
  }

#undef SWITCH_PLAYER

  short board::size_of_component (short field, player::player player)
  {
    short stack[61];
    short pos (0);
    short size (0);

    if (!SEEN (field))
    {
      stack[pos++] = field;
      SEE (field);
      ++size;
    }

    while (pos)
    {
      short const f (stack[--pos]);

      for ( short npos (neighbour_begin[f])
          ; npos < neighbour_begin[f + 1]
          ; ++npos
          )
      {
        short const n (neighbour[npos]);

        if (!SEEN (n) && IS_TAKEN (player, n))
        {
          stack[pos++] = n;
          SEE (n);
          ++size;
        }
      }
    }

    return size;
  }

#define CHILD()                                                 \
  std::pair<uint64_t, player::player> const child               \
    (size_of_tree_for_winning_move (d + 1));                    \
                                                                \
  size += child.first;                                          \
                                                                \
  if (d == 0 && child.second == 1 - _to_move)                   \
  {                                                             \
    std::cout << show (*this) << '\n';                          \
  }

#define RETURN_ON_WINNING_MOVE()                                        \
  if (child.second == _to_move)                                         \
  {                                                                     \
    _cache.put (key, value, _to_move, _available_stones, _to_move);     \
                                                                        \
    return {size,_to_move};                                             \
  }

  std::pair<uint64_t, player::player> board::size_of_tree_for_winning_move
    (short d)
  {
    uint64_t size (1);

    if (_high_water[1 - _to_move] > _free_fields + _high_water[_to_move])
    {
      ++_high_water_other;

      return {size, 1 - _to_move};
    }

    if (_high_water[_to_move] > _free_fields + _high_water[1 - _to_move])
    {
      ++_high_water_to_move;

      return {size, _to_move};
    }

    uint64_t key (0);
    uint64_t value[2] = {0,0};

    for (int field (0); field < 32; ++field)
    {
      key ^= _magic[61 * _taken[field] + field];
      value[0] += _taken[field];
      value[0] <<= 2;
    }
    for (int field (32); field < 61; ++field)
    {
      key ^= _magic[61 * _taken[field] + field];
      value[1] += _taken[field];
      value[1] <<= 2;
    }

    if (_cache.get (key, value, _to_move, _available_stones) == _to_move)
    {
      return {size, _to_move};
    }

    if (_free_fields == 0)
    {
      ++_leaf;

      player::player const result (in_front());

      _cache.put (key, value, _to_move, _available_stones, result);

      return {size, result};
    }

    const short available_stones (_available_stones);
    const short high_water = _high_water[_to_move];

    if (_available_stones > 2)
    {
      for (short f (0); f < 61 - 2; ++f)
      {
        if (IS_FREE (f))
        {
          for (short g (f + 1); g < 61 - 1; ++g)
          {
            if (IS_FREE (g))
            {
              for (short h (g + 1); h < 61; ++h)
              {
                if (IS_FREE (h))
                {
                  put (f, g, h);
                  CHILD();
                  unput (f, g, h, high_water, available_stones);
                  RETURN_ON_WINNING_MOVE();
                }
              }
            }
          }
        }
      }
    }

    if (_available_stones > 1)
    {
      for (short f (0); f < 61 - 1; ++f)
      {
        if (IS_FREE (f))
        {
          for (short g (f + 1); g < 61; ++g)
          {
            if (IS_FREE (g))
            {
              put (f, g);
              CHILD();
              unput (f, g, high_water, available_stones);
              RETURN_ON_WINNING_MOVE();
            }
          }
        }
      }
    }

    if (_available_stones > 0)
    {
      for (short f (0); f < 61; ++f)
      {
        if (IS_FREE (f))
        {
          put (f);
          CHILD();
          unput (f, high_water, available_stones);
          RETURN_ON_WINNING_MOVE();
        }
      }
    }

    _cache.put (key, value, _to_move, _available_stones, 1 - _to_move);

    return {size, 1 - _to_move};
  };

#undef RETURN_ON_WINNING_MOVE
#undef CHILD

  player::player board::in_front()
  {
      short size[2][61];
      short top[2] = {0,0};

      UNSEE_ALL();

      for (player::player player : {BLUE, ORANGE})
      {
        for (short field (0); field < 61; ++field)
        {
          if (!SEEN (field) && IS_TAKEN (player, field))
          {
            size[player][top[player]++] = size_of_component (field, player);
          }
        }

        std::sort ( size[player], size[player] + top[player]
                  , std::greater<short>()
                  );
      }

      short pos[2] = {0,0};

#define VALID(player) (pos[player] != top[player])
#define SIZE(player) size[player][pos[player]]
#define INC(player) ++pos[player]

      while (VALID (BLUE) && VALID (ORANGE) && (SIZE (BLUE) == SIZE (ORANGE)))
      {
        INC (BLUE);
        INC (ORANGE);
      }

      if (VALID (BLUE) && VALID (ORANGE))
      {
        return (SIZE (BLUE) > SIZE (ORANGE)) ? BLUE : ORANGE;
      }

      if (VALID (BLUE))
      {
        return BLUE;
      }

      if (VALID (ORANGE))
      {
        return ORANGE;
      }

#undef INC
#undef SIZE
#undef VALID

    abort();
  };

#undef IS_FREE
#undef IS_TAKEN
#undef UNTAKE
#undef TAKE

#undef SEE
#undef SEEN
#undef UNSEE_ALL

#undef NONE
#undef ORANGE
#undef BLUE
}

void lg1657873 (board* b)
{
  b->put (23);
  b->put (31,47);
  b->put (30,46);
  b->put (38,21);
  b->put (14,20);
  b->put (28,15);
  b->put (29,39);
  b->put (40,22,13);
  b->put (53,54,7);
  b->put (10,5);
  b->put (36,49);
  b->put (37,33);
  b->put (44,6);
  b->put (12,19);
  b->put (32,41,48);
  b->put (52,17);
  b->put (16,9);
  b->put (8,3,45);
  b->put (51,57,58);
  b->put (4,27,35);
  // b->put (43,24,25);
  // b->put (0,1);
  // b->put (2,59,34);
  // b->put (18,11);
  // b->put (42,55,60);
  // b->put (26,50,56);
}

void lg1659736 (board* b)
{
  b->put (16);
  b->put (23,37);
  b->put (22,38);
  b->put (30,20);
  b->put (28,40);
  b->put (29,15);
  b->put (31,14,52);
  b->put (8,32);
  b->put (45,44);
  b->put (48,54);
  b->put (41,39);
  b->put (7,12,33);
  b->put (13,6);
  b->put (5,0,1);
  b->put (36,19,42);
  b->put (27,34,53);
  b->put (9,3);
  b->put (24,58);
  //  b->put (11, 57);
  //  b->put (35, 18, 17);
}

void lg1659772 (board* b)
{
  b->put (30);
  b->put (14,20);
  b->put (37,33);
  b->put (23,40);
  b->put (15,13);
  b->put (46,51);
  b->put (48,53);
  b->put (32,47);
  b->put (45,52,22);
  b->put (21,8);
  b->put (16,41);
  b->put (28,44);
  b->put (19,12);
  b->put (57,58);
  b->put (36,7);
  b->put (43,10);
  b->put (54,27);
  b->put (9,24,17);
  b->put (29,2);
  b->put (25,34,42);
  b->put (49,3);
}

int main()
{
  board b (1UL << 30);

  lg1659736 (&b);

  std::cout << show (b) << '\n';

  std::pair<uint64_t, player::player> const winner
    (b.size_of_tree_for_winning_move());

  std::cout << "winner: " << player::show (winner.second) << '\n';
  std::cout << "size_of_tree_for_winning_move: " << winner.first << '\n';

  return 0;
}

/*
cache: size 1073741824 boards 44739242 positions 178956968
to_move O high_water 16 15 available_stones 2 free_fields 23
     O O . B .
    O B O O B .
   . O B B O B .
  . B O . B O . .
 . O B O O B O O O
  . B O B B B B B
   . B B . . O .
    . . B O O .
     . . . . .

winner: B
size_of_tree_for_winning_move: 236317516
cache: put_board 8765754 put_position 13544617 hit 177257329 miss_board 8765756 miss_position 3883637
catchup5.exe: 98.76 sec(s)

cache: size 1073741824 boards 44739242 positions 178956968
to_move O high_water 14 5 available_stones 3 free_fields 27
     . . . . .
    . . B O . O
   . B B O B B .
  . B O O B O . .
 . B O . B . O B .
  . B B . . O B .
   O O B O O B .
    . O B B B .
     . O O . .

winner: B
size_of_tree_for_winning_move: 15233973494
cache: put_board 361320941 put_position 492679877 hit 11111748204 miss_board 361320978 miss_position 47905387
catchup5.exe: 8279.94 sec(s)
*/
