.PHONY: build clean

# Build the compiler and point ./whelm at the fresh binary.
build:
	cabal build exe:elm
	ln -sf $$(realpath --relative-to=. $$(cabal list-bin exe:elm)) whelm

clean:
	cabal clean
	rm -f whelm
