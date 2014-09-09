
CXXFLAGS += -std=c++11
CXXFLAGS += -Wall
CXXFLAGS += -Wextra
CXXFLAGS += -O3

Catchup.exe: Catchup.o
	$(CXX) -o $@ $^ $(LDFLAGS)

%.run: %.exe
	@/usr/bin/time -f '$^: %e sec(s)' ./$^

MAIN = Main

$(MAIN): $(wildcard *.hs) Makefile
	ghc --make -O3 $(MAIN).hs

.PHONY: clean

clean:
	$(RM) *.o
	$(RM) *.hi
	$(RM) *.prof
	$(RM) $(MAIN)
	$(RM) Catchup.exe
