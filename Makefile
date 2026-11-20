# `make` is the canonical way to build: it stamps the build number, the
# commit and whether the checkout is clean into gen/BuildCount.hs (see
# installers/stamp-build-count.sh) and builds the compiler. A bare
# `cabal build exe:elm` also works once gen/ exists, but keeps the last
# stamp. A clean checkout's commit names the compiled caches
# (elm-stuff/webbhuset-<version>-<commit>/), so a binary built that way
# after editing a clean checkout reuses that commit's compiled output.

.PHONY: build clean

build: gen/BuildCount.hs
	cabal build exe:elm
	ln -sf $$(realpath --relative-to=. $$(cabal list-bin exe:elm)) whelm

# Runs on every make; the file is rewritten only when the stamp changes, so
# unchanged builds stay warm. The release workflow runs the same script, so
# both stamp identically.
.PHONY: gen/BuildCount.hs
gen/BuildCount.hs:
	@sh installers/stamp-build-count.sh

clean:
	cabal clean
	rm -rf gen
	rm -f whelm
