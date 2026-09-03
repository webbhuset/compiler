#!/bin/sh
# Stamps the build number (the commit count of HEAD) into gen/BuildCount.hs.
#
# Shared by the Makefile and the release workflow so both stamp the same way.
# Requires a full clone: a shallow checkout only counts the commits it fetched
# (CI must use actions/checkout with fetch-depth: 0).
#
# The file is rewritten only when the count changes, so unchanged builds stay
# warm.

set -eu

cd "$(dirname "$0")/.."

mkdir -p gen
count=$(git rev-list --count HEAD 2>/dev/null || echo unknown)
printf 'module BuildCount (count) where\n\n\ncount :: String\ncount =\n  "%s"\n' "$count" > gen/BuildCount.hs.tmp

if cmp -s gen/BuildCount.hs.tmp gen/BuildCount.hs; then
  rm gen/BuildCount.hs.tmp
else
  mv gen/BuildCount.hs.tmp gen/BuildCount.hs
  echo "Stamped build $count"
fi
