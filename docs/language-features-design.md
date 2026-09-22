# Language Features — Design Proposal

**Status: proposal. Nothing below is implemented.**

This fork adds language features that the official compiler cannot parse or
type. A project should be able to say which of them it accepts, so that a
codebase can stay on plain Elm, or take one feature and not the rest, without
needing a different compiler binary.

The proposal: a `"language-features"` field in `elm.json`, empty by default,
checked once per project. A project that lists nothing compiles the exact
language the official compiler does.


## Why not a branch per feature set

This is what the fork does today, and it does not scale. The `fusion` branch
exists only because someone wanted `explore` minus overloading, back-lambdas
and structural variants; it was assembled by cherry-pick and still carries an
unresolved conflict between the scripts and variants work. That is one subset
out of the nine features below. Every bugfix has to be backported to each
branch, and each branch needs its own release binary.


## Why not a CLI flag

A flag was considered and rejected on three counts.

- **Stale artifacts.** `Elm.Details.load` re-reads `elm.json` only when its
  mtime changes (`builder/src/Elm/Details.hs:166`), and regenerating resets
  `_locals` to `Map.empty` (`:340`), which forces a full rebuild of the
  project's own modules. A flag gets none of that: build with the flag, build
  again without it, and the cached `.elmi`/`.elmo` survive. The flag would
  have to be folded into the `Details` fingerprint to be correct at all.
- **It would only cover `elm make`.** `elm repl`, `elm reactor`, elm-test and
  the editor integrations all go through `Details.load` and `Build`, and none
  of them will pass a flag along. The restriction would silently not apply
  in the places where code is actually written.
- **A restriction that is lifted by typing a flag is not a restriction.**
  Which language a project is written in is a property of the project.


## Why not per-module pragmas

Haskell's `{-# LANGUAGE #-}` is per-module opt-in, which is the opposite of
what is wanted here: any file could switch overloading back on. Pragmas
document where a feature is used, but they cannot restrict it. The same
documentation falls out of the error messages instead.


## The field

```json
{
    "type": "application",
    "source-directories": [ "src" ],
    "elm-version": "0.19.2",
    "dependencies": { "direct": {}, "indirect": {} },
    "test-dependencies": { "direct": {}, "indirect": {} },
    "language-features": [ "back-lambdas", "structural-variants" ]
}
```

Valid in both application and package `elm.json`. Absent or empty means no
feature is enabled. An unknown string in the list is an error, so a typo does
not silently disable something.

`elm.json` stays valid for the official tooling, because `Json.Decode` here is
field-based and ignores keys it does not know. This is the same arrangement
`"git-dependencies"` already uses: decoded at `builder/src/Elm/Outline.hs:334`
and written back only when non-empty (`:173`).


## The features

The list splits in two, along one line: whether the feature has a footprint in
`Elm.Interface`, which is what actually crosses a package boundary.

### Module-local

Nothing about these escapes the module that writes them.

| id | syntax or trigger | recognised at |
|---|---|---|
| `back-lambdas` | `\x <- source` | `compiler/src/Parse/Expression.hs:450` |
| `css-blocks` | `[css\| ... \|]` blocks | `Src.Css` → `Can.Css` |
| `async-imports` | `import async M` | `Src.Async` in an import |
| `task-ports` | `port f : X -> Task e a` | `Canonicalize/Effects.hs:144` |
| `scripts` | `main : System.Process -> Task String Int` | `Opt.Script`, `Optimize/Module.hs:305` |
| `workers` | `main : Workers.Program ...`, `Worker.spawn` | `Opt.Worker` + `Nitpick.Workers` |

Each stops at the boundary for a concrete reason: a back-lambda desugars into
`Src.Call` and `Src.Lambda` during parsing, a css block's value has an ordinary
type from the `webbhuset/css` package, `Can._asyncs` is per-module and is never
serialised into an interface, ports are application-only already, and the two
`main` shapes are only meaningful in a compilation root.

### Interface-visible

These are exactly the three fields the fork added to or changed in
`compiler/src/Elm/Interface.hs`.

| id | syntax or trigger | footprint |
|---|---|---|
| `structural-variants` | `tag` declarations, tag literals and patterns, `widen` | `I._tags`, `Can.Widen` |
| `overloading` | `abstract` declarations, qualified definitions, `where` clauses | `I._overloads` |
| `comparable-newtypes` | **no syntax at all** | `I._comparables` |

`comparable-newtypes` is the extreme case: a single-constructor,
single-field type simply *is* comparable, and no source anywhere mentions the
feature. It shows up only as unification succeeding where it otherwise would
not.


## Lists are never inherited

A project's list governs that project's own source. It says nothing about its
dependencies, and a dependency's list says nothing about it.

The tempting alternative — a dependency declares an interface-visible feature,
and every consumer's list must be a superset — was rejected. It hands one
package the power to decide what language its consumers write: expose a tag row
in one corner of an API and every application downstream is forced to enable
`structural-variants`. It would also make strict-by-default worthless from day
one, since `elm init` pins an `elm/core` fork that exposes `Basics.widen`.

What happens instead, when an application has a feature disabled and a
dependency's API uses it:

- `structural-variants` — the application cannot write tag syntax, or a tag
  row in an annotation. It can still receive a tag value from a package and
  pass it along, as long as it never names the type. `Basics.widen` being
  exposed costs nothing if it is never called.
- `overloading` — canonicalization emits `Can.VarOverload` at every use of an
  overloaded name, so using a dependency's overloaded name is rejected at the
  use site, with a region pointing at it.
- `comparable-newtypes` — inference does not consult the comparables table, so
  using a package's newtype as a `Dict` key produces the ordinary upstream
  *"I need a comparable here"* error.

In each case the feature being off costs the project access to part of an API,
and it finds out at a specific place in its own source. It is never forced
into a language it did not ask for.


## Where the checks live

The threading already exists: `Parse.fromByteString` takes a `ProjectType` and
uses it for exactly this kind of gating, and `checkAsyncImports`
(`compiler/src/Parse/Module.hs:98`) already rejects `import async` in a package
after parsing, with its own error. That is the pattern to generalise.

1. **`compiler/src/Elm/Feature.hs`** (new) — `data Feature`, a set wrapper,
   and `fromChars`/`toChars` for `elm.json` and error messages.
2. **`Elm.Outline`** — `_app_features` and `_pkg_features`, mirroring
   `git-dependencies`.
3. **Threading** — bundle `ProjectType` and the feature set into one
   `Parse.Env` record. `Build.makeEnv` (`builder/src/Build.hs:77-86`) already
   destructures `Details` and has the outline to hand; there are four
   `Parse.fromByteString` call sites (`Build.hs:315, 375, 391, 895`).
4. **Module-local features** — one pass over `Src.Module` after parsing and
   before canonicalization, reporting every use of a disabled feature with its
   region and the string to add to `elm.json`. The grammar keeps accepting
   everything, so all of the gating lives in one file.
   `task-ports`, `scripts` and `workers` are decided by a type rather than by
   syntax, so those three are checked where they are recognised today, in
   canonicalization and in `Optimize.Module`.
5. **Interface-visible features** — later in the pipeline. `overloading` at
   `Can.VarOverload` and at `abstract`/`where` declarations;
   `structural-variants` at tag declarations, tag expressions and patterns,
   tag rows in annotations, and `Can.Widen`; `comparable-newtypes` at the
   point where the unifier consults the table.

The new keywords are contextual: `reservedWords`
(`compiler/src/Parse/Variable.hs:63`) is untouched, so `tag`, `abstract` and
`async` remain usable as ordinary names and no program that used to compile
changes meaning when a feature is switched off.

### One wart

`Type.Comparable` keeps its table in a process-global `IORef`, because the
unifier deliberately has no environment to carry it. The gate therefore has to
sit at the consult site (`comparablePositions`) and read a build-scoped global
set once from the outline — not at registration, since dependency interfaces
are registered either way. This is correct because one `elm make` compiles the
source of exactly one project, and it follows the approach the trusted-kernel
table already uses.


## Strict by default

A project with no `"language-features"` gets plain Elm. Existing projects on
this fork need one line added to `elm.json`, and `elm init` writes the list
that the pinned forks require.

This is a breaking change for projects already on the fork, and it is the right
default anyway: the fork becomes a strict superset that is opted into per
project, and being restricted is what happens when nobody configures anything.


## Out of scope

Not language features, and not part of this field:

- `"git-dependencies"`, which already has its own field.
- ES module output, decided by the `--output` extension.
- HTTP over fetch and HTML-to-string, which are packages the compiler knows
  nothing about — controlled by what a project depends on.
- Serving compiled pieces from the reactor, the extensible-record inference
  fix, cross-platform release binaries.

**Kernel code in git dependencies** deserves a setting, but not this one.
Whether a package pulled from git may define `Elm.Kernel.*` modules, effect
managers and custom infix operators is a trust decision about dependencies, not
a statement about what language this project is written in. It wants its own
key.

There is no publish-time check, because this fork does not publish: the
`publish` command is removed from the CLI, so that no fork-only feature can
reach <https://package.elm-lang.org> by accident. A package meant for the
official registry is built with the official compiler.
