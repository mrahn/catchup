
INCLUDEPATH += $(PWD)
INCLUDEPATH += $(PWD)/../include

CXXFLAGS += -std=c++11
CXXFLAGS += -Wall
CXXFLAGS += -Wextra
CXXFLAGS += -O3

CXXFLAGS += $(addprefix -I , $(INCLUDEPATH))

main.o: board.hpp board.cpp
main.o: $(PWD)/../include/player.hpp
main.o: $(PWD)/../include/stream_modifier.hpp

%.exe: %.o
	$(CXX) -o $@ $^ $(LDFLAGS)

%.run: %.exe
	@/usr/bin/time -f '$^: %e sec(s)' ./$^

.PHONY: clean

clean:
	$(RM) *.o
	$(RM) *.prof
	$(RM) *.exe
