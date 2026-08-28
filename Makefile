# `make` is the canonical way to build: it stamps the build number
# (the commit count of HEAD) into gen/BuildCount.hs and builds the
# compiler. A bare `cabal build exe:elm` also works once gen/ exists,
# but reuses the last stamped number.

.PHONY: build clean

build: gen/BuildCount.hs
	cabal build exe:elm
	ln -sf $$(realpath --relative-to=. $$(cabal list-bin exe:elm)) whelm

# Rewritten only when the count changes, so unchanged builds stay warm.
.PHONY: gen/BuildCount.hs
gen/BuildCount.hs:
	@mkdir -p gen
	@count=$$(git rev-list --count HEAD 2>/dev/null || echo unknown); \
	printf 'module BuildCount (count) where\n\n\ncount :: String\ncount =\n  "%s"\n' "$$count" > gen/BuildCount.hs.tmp; \
	if cmp -s gen/BuildCount.hs.tmp gen/BuildCount.hs; \
	  then rm gen/BuildCount.hs.tmp; \
	  else mv gen/BuildCount.hs.tmp gen/BuildCount.hs; echo "Stamped build $$count"; \
	fi

clean:
	cabal clean
	rm -rf gen
	rm -f whelm
