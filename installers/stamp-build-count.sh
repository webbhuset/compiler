#!/bin/sh
# Stamps gen/BuildCount.hs with:
#
# - cleanCommit: the commit HEAD names, when the checkout is clean; Nothing
#   otherwise. It names the compiled caches (see BuildId in
#   builder/src/Root.hs).
# - description: the build number (the commit count of HEAD), the commit, the
#   tag at a tagged commit, and whether the checkout was clean.
#   Shown by `elm --help` and written into each elm-stuff cache folder; it is
#   only a label, nothing reads it back.
#
# A checkout is clean when git's top level is this directory and `git status`
# shows nothing, untracked files included. Not clean: uncommitted changes, a jj
# workspace nested inside another checkout (git there describes the outer
# one), or no git at all. Those builds are dev builds, which name their caches
# after their own binary instead.
#
# Shared by the Makefile and the release workflow so both stamp the same way.
# Requires a full clone: a shallow checkout only counts the commits it fetched
# (CI must use actions/checkout with fetch-depth: 0).
#
# The file is rewritten only when the stamp changes, so unchanged builds stay
# warm.

set -eu

cd "$(dirname "$0")/.."

mkdir -p gen

count=$(git rev-list --count HEAD 2>/dev/null || echo unknown)
commit=$(git rev-parse HEAD 2>/dev/null | cut -c1-12)
commit=${commit:-unknown}
tag=$(git describe --tags --exact-match HEAD 2>/dev/null || true)

# --show-prefix is empty exactly when this directory is git's top level. It
# avoids comparing paths, which git and the shell spell differently on Windows.
if prefix=$(git rev-parse --show-prefix 2>/dev/null) \
  && [ -z "$prefix" ] \
  && changes=$(git status --porcelain 2>/dev/null) \
  && [ -z "$changes" ] \
  && [ "$commit" != unknown ]
then
  clean_commit="Just \"$commit\""
  state=""
else
  clean_commit="Nothing"
  state=", not a clean checkout"
fi

description="build $count, $commit${tag:+, $tag}$state"

# Escaped for the Haskell string literal: git allows `"` and `\` in tags.
description_literal=$(printf '%s' "$description" | sed 's/[\\"]/\\&/g')

printf 'module BuildCount (cleanCommit, description) where\n\n\ncleanCommit :: Maybe String\ncleanCommit =\n  %s\n{-# NOINLINE cleanCommit #-}\n\n\ndescription :: String\ndescription =\n  "%s"\n{-# NOINLINE description #-}\n' "$clean_commit" "$description_literal" > gen/BuildCount.hs.tmp

if cmp -s gen/BuildCount.hs.tmp gen/BuildCount.hs; then
  rm gen/BuildCount.hs.tmp
else
  mv gen/BuildCount.hs.tmp gen/BuildCount.hs
  echo "Stamped $description"
fi
