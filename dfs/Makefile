
CXXFLAGS += -std=c++1z
CXXFLAGS += -Wall
CXXFLAGS += -Wextra
CXXFLAGS += -O3
CXXFLAGS += -fno-exceptions

%.exe: %.o
	$(CXX) -o $@ $^ $(LDFLAGS)

%.run: %.exe
	@/usr/bin/time -f '$^: %e sec(s)' ./$^

.PHONY: clean

clean:
	$(RM) *.o
	$(RM) *.exe
