
MAIN = Main

$(MAIN): $(wildcard *.hs) Makefile
	ghc --make -O3 $(MAIN).hs

.PHONY: clean

clean:
	$(RM) *.o
	$(RM) *.hi
	$(RM) *.prof
	$(RM) $(MAIN)
