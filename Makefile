
INCLUDEPATH += $(PWD)

CXXFLAGS += -std=c++11
CXXFLAGS += -Wall
CXXFLAGS += -Wextra
CXXFLAGS += -O3

CXXFLAGS += $(addprefix -I , $(INCLUDEPATH))

catchup.o: stream_modifier.hpp
catchup.o: point.hpp point.cpp
catchup.o: player.hpp player.cpp

catchup.exe: catchup.o
	$(CXX) -o $@ $^ $(LDFLAGS)

%.run: %.exe
	@/usr/bin/time -f '$^: %e sec(s)' ./$^

.PHONY: clean

clean:
	$(RM) *.o
	$(RM) *.prof
	$(RM) catchup.exe
