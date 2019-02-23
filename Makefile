Main: *.hs
	ghc -threaded -Wall Main

all: clean Main

.PHONY: run
run: Main midi2osc.conf
	./Main midi2osc.conf

.PHONY: clean
clean:
	-rm *.hi
	-rm *.o
	-rm Main
	-rm Main.exe

