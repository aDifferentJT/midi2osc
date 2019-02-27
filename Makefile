Main: *.hs
	ghc -threaded -Wall -hide-package "base" -package "base-noprelude" -package "base (Prelude as BasePrelude)" Main

.PHONY: i-%
i-%: *.hs
	ghci -threaded -Wall -hide-package "base" -package "base-noprelude" -package "base (Prelude as BasePrelude)" $*

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

