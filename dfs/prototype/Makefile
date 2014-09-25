
MAIN = Main

%.exe: $(wildcard *.hs) Makefile
	ghc --make -O3 $*.hs -o $*.exe

.PHONY: clean

clean:
	$(RM) *.o
	$(RM) *.hi
	$(RM) *.prof
	$(RM) *.exe
