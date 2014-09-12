
INCLUDEPATH += $(PWD)

CXXFLAGS += -std=c++11
CXXFLAGS += -Wall
CXXFLAGS += -Wextra
CXXFLAGS += -O3

CXXFLAGS += $(addprefix -I , $(INCLUDEPATH))

catchup.o: board.hpp board.cpp
catchup.o: constant.hpp
catchup.o: neighbourhood.hpp
catchup.o: $(PWD)/../include/player.hpp
catchup.o: $(PWD)/../include/stream_modifier.hpp

%.exe: %.o
	$(CXX) -o $@ $^ $(LDFLAGS)

%.run: %.exe
	@/usr/bin/time -f '$^: %e sec(s)' ./$^

.PHONY: clean

clean:
	$(RM) *.o
	$(RM) *.prof
	$(RM) *.exe
