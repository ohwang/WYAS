# Makefile for WYAS

main: *.hs
	ghc Main.hs -o main
	@echo cleaning ...
	@rm *.hi *.o

clean:
	rm -v *.hi *.o main test

