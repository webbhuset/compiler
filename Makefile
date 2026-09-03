# `make` is the canonical way to build: it stamps the build number
# (the commit count of HEAD) into gen/BuildCount.hs and builds the
# compiler. A bare `cabal build exe:elm` also works once gen/ exists,
# but reuses the last stamped number.

.PHONY: build clean

build: gen/BuildCount.hs
	cabal build exe:elm
	ln -sf $$(realpath --relative-to=. $$(cabal list-bin exe:elm)) whelm

# Rewritten only when the count changes, so unchanged builds stay warm. The
# release workflow runs the same script, so both stamp identically.
.PHONY: gen/BuildCount.hs
gen/BuildCount.hs:
	@sh installers/stamp-build-count.sh

clean:
	cabal clean
	rm -rf gen
	rm -f whelm
