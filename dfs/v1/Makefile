
INCLUDEPATH += $(PWD)
INCLUDEPATH += $(PWD)/../include

CXXFLAGS += -std=c++11
CXXFLAGS += -Wall
CXXFLAGS += -Wextra
CXXFLAGS += -O3

CXXFLAGS += $(addprefix -I , $(INCLUDEPATH))

%.o: board.hpp board.cpp
%.o: constant.hpp
%.o: neighbourhood.hpp
%.o: $(PWD)/../include/player.hpp
%.o: $(PWD)/../include/stream_modifier.hpp
%.o: $(PWD)/../include/hash.hpp

%.exe: %.o
	$(CXX) -o $@ $^ $(LDFLAGS)

%.run: %.exe
	@/usr/bin/time -f '$^: %e sec(s)' ./$^

.PHONY: clean

clean:
	$(RM) *.o
	$(RM) *.prof
	$(RM) *.exe
