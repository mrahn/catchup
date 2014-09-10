
CXXFLAGS += -std=c++11
CXXFLAGS += -Wall
CXXFLAGS += -Wextra
CXXFLAGS += -O3

Catchup.exe: Catchup.o
	$(CXX) -o $@ $^ $(LDFLAGS)

%.run: %.exe
	@/usr/bin/time -f '$^: %e sec(s)' ./$^

.PHONY: clean

clean:
	$(RM) *.o
	$(RM) *.prof
	$(RM) Catchup.exe
