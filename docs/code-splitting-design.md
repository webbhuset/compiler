# Code Splitting — Design Notes

**Status: implemented. Compiler side on this branch; the runtime is two
patches to the forked packages (`patches/elm-core-code-splitting.patch`,
`patches/elm-browser-code-splitting.patch`). Requires `--output=*.mjs`.**

Everything reachable from `main` ships in one bundle and is evaluated
before the app starts, including the page the user never opens. There is no
way to say "this module's code can arrive later." This feature adds one:

```elm
import async Pages.Report


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenReport ->
            -- the import() happens here, the first time
            Pages.Report.open model
```

`Pages.Report` and everything only it needs are emitted to a separate file.
`elm make src/Main.elm --output=app.mjs` writes `app.mjs` plus one
`app.<hash>.mjs` per async-imported module.


## The problem: `import()` is async and Elm is not

`Pages.Report.open model` has type `( Model, Cmd Msg )` — a synchronous
value — while `import()` yields a promise. Something has to bridge that,
and the choice of bridge decides the whole shape of the feature.

Three were considered.

**Type the boundary.** Make `import async M` bring each `M.v : t` into
scope as `Task Never t`. Honest, visible in the types, no runtime
machinery, and it composes with `Task.await`. But every use site then goes
through a `Cmd` round-trip, so a `view` cannot be loaded lazily at all —
you load the page's API into your model and thread it everywhere. It makes
the common case (a route's worth of code) awkward, and it changes what an
import means.

**Split at a value, not an import.** `Async.load Pages.Report.page`, with
the argument required to be a top-level name, exactly as
`Worker.spawn Counter.main` already works. Smallest diff by far and fully
typed. But one loadable value per chunk, threaded through the model, and
`import` — the thing that actually expresses "my code depends on that
code" — stays uninvolved.

**Suspend and retry.** Chosen. A reference to a not-yet-loaded chunk throws
a sentinel. The runtime catches it at the handful of places where it calls
user code, awaits the chunk, and calls again.

This is sound *because Elm is pure*. When the throw escapes `update`,
nothing has been committed: no model assigned, no effects enqueued, no DOM
touched. Re-running `update` on the same message and the same model is
indistinguishable from having run it once. The same argument covers `init`,
`view` and `subscriptions` — every entry point the runtime has into user
code is a pure function called from a place that can simply call it again.

So the type system is untouched, `import async M` brings exactly the same
names into scope as `import M`, and the feature is invisible at the use
site. That invisibility is the point: it is what lets an existing module be
split off by editing one line, and it is also the main thing to be uneasy
about — see *What is checked, what is not*.


## Why this fork can do it

Three pieces already exist and are reused rather than rebuilt:

- **ESM output** (`docs/esm-output.md`). A chunk URL has to be resolved
  relative to the bundle that names it, and only a module knows its own
  URL. Async imports require `--output=*.mjs` for the same reason workers
  do.
- **Multi-bundle output with content-hashed siblings**
  (`builder Generate.finalize`). Worker bundles are already rendered,
  hashed, named `<base>.<hash16>.mjs`, and have their names substituted
  back into the bundles that reference them.
- **`Opt.WorkerRef`** — a reference to a global that deliberately
  registers *no* dependency, so the referenced code stays out of this
  bundle, leaving a NUL-delimited placeholder token where the file name
  goes. `Opt.AsyncRef` is the same trick.

And one argument carries over wholesale: both bundles are produced by one
`elm make`, so under `--optimize` they share a single field-rename table.
Representations agree across the boundary by construction.

The genuinely new ingredient is that a chunk **shares the main bundle's
scope** instead of being self-contained. Worker bundles duplicate whatever
they share with the spawner, which is fine across realms. Inside one realm
it is not: two copies of a function break `Html.Lazy`'s reference
comparison, and two copies of an effect manager break `_Platform_`'s
singleton registry.


## Decisions worth recording

- **Async-ness is a property of the reference, not the module.** If `Main`
  says `import async Big` while some other module says `import Big`, then
  `Big` is synchronously reachable and must be in the main bundle. The
  async import then correctly degrades to nothing. Keeping the flag on the
  reference makes that fall out instead of needing a special case.
- **Only values defer.** Constructors, `VarEnum`, `VarBox` — anything a
  `case` needs — stay eager. They are tiny, they are often inlined to an
  integer under `--optimize`, and a suspend in the middle of pattern
  matching would be a genuinely surprising place to pause.
- **Code two chunks both need is hoisted into main, not duplicated.**
  Forced by the shared-realm argument above. It also means chunk contents
  depend on the whole program, which is the right tradeoff: it is what
  keeps `==` on a shared value meaningful.
- **Effect managers always ship in main.** `_Platform_setupEffects`
  snapshots `_Platform_effectManagers` at init; a manager arriving later
  could never receive a command. So an `effect module` used only by an
  async page still costs its bytes up front. Lifting this means making
  manager setup lazy, which is a separate change to the core kernel.
- **Kernel chunks ship in main.** They are emitted as a prelude block
  ahead of everything else (`State._revKernels`), and splitting that
  ordering across files buys little.
- **A chunk is named by its module, and hashed like a worker bundle.** A
  chunk that references another chunk has to be hashed after it, so chunks
  are emitted in dependency order and a chunk cycle is a compile error —
  the same restriction, for the same reason, as spawn cycles. Two pages
  that `import async` each other is the shape that hits this; it is rare
  because a router usually names every page from one module.
- **No preloading, no hints, in this version.** Loading starts at the
  first reference and not before. `<link rel=modulepreload>` and an
  explicit "warm this chunk" command are obvious follow-ups, but they are
  additions to a working mechanism rather than part of it.


## Compile pipeline

1. **Parse** (`Parse.Module.chompImport`): `import async M`, with optional
   `as` and `exposing` exactly as before. Follows the
   `port module` / `effect module` precedent — `async` is a positional
   keyword, *not* added to `reservedWords`, so it stays usable as a
   variable name. `Src.Import` gains a field.
2. **Canonicalize**: unchanged. `import async M` builds the same
   environment as `import M`; type inference never learns about this. The
   set of async-imported modules rides along on `Can.Module`, next to
   `_effects`.
3. **Validation** (`Nitpick.AsyncImports`, run from `Compile.compile`): an
   async reference may not appear where it would be forced at bundle load.
   See below.
4. **Rewrite** (`Optimize.Expression`): a `Can.VarForeign home name` whose
   `home` is async-imported compiles to `Opt.AsyncRef global` (binary tag
   29) and registers no dependency. The chunk a reference belongs to is
   the global's `home`.
5. **Planning** (`Generate.Chunks.plan`, modelled on `Generate.Workers`):
   walk the live graph from the mains *without* crossing `AsyncRef`s —
   that is `mainLive`. Then, per chunk, walk from its referenced globals.
   A chunk owns what is live for it, minus `mainLive`, minus anything a
   second chunk also wants (hoisted to main), minus `Manager` and `Kernel`
   nodes (always main). Order chunks depth-first; a cycle is an error.
6. **Codegen**: `Opt.AsyncRef (Global h n)` emits
   `_Chunk_get(_Chunk$author$project$Big).$author$project$Big$n`. Each
   chunk is walked with the main bundle's globals already marked as seen,
   so it stops at the boundary and emits only its own. The main bundle
   gains a registration per chunk holding that chunk's placeholder token,
   and one `_Chunk_scope()` returning the names the chunks need.
7. **Finalize** (`builder Generate.finalize`): unchanged in shape from the
   worker path — render in dependency order, substitute the names of the
   chunks each chunk references, SHA-1, name `<base>.<hash16>.mjs`,
   substitute everything into the main bundle. `elm reactor` uses
   `finalizeWith` and serves each chunk at its own module's URL, as it
   already does for workers.

A chunk file is a module with one default export:

```js
export default function(__scope) {
  var $author$project$Shared$helper = __scope.$author$project$Shared$helper;
  // ... the chunk's own definitions ...
  return { $author$project$Big$open: $author$project$Big$open, ... };
}
```

Rebinding the shared names as locals at chunk init keeps every reference
inside the chunk a plain identifier, so chunk code pays no per-access cost
for living in another file. Main pays one property lookup per async
reference.


## Which names cross the boundary

Working out that list is the one part of this that is not a graph
question, and it is worth saying why.

Most of what a chunk needs from the main bundle is in the dependency sets
and could be read off the graph. Kernel code is not. It arrives as raw
JavaScript, so `_Utils_update`, `_List_fromArray`, `_VirtualDom_node` and
the rest are defined nowhere the compiler can enumerate -- and the code
generator emits some of them inline, from `Opt.Tuple` or `Opt.List`,
without any dependency being registered at all. Any list assembled from
the graph would be missing exactly the names that are hardest to notice
are missing.

So the question is asked of the generated text instead
(`Generate.JavaScript.Scope`), in two deliberately asymmetric halves:

- **What the main bundle defines**: definitions at column zero of its
  rendered text. Everything the compiler emits at the top level of a
  bundle starts there and everything nested is indented, so this is exact.
  It must be: a name offered that does not exist would throw when the
  scope object is built.
- **What a chunk mentions**: every identifier anywhere in its rendered
  text. This is allowed to over-report -- a word inside a string literal
  costs one unused `var` and nothing else.

A chunk takes the intersection. The main bundle's scope object is the
union of what the chunks took.


## Runtime

Two parts, split so that a program with no async imports produces the bytes
it produces today.

**Emitted by the compiler**, only when the program has chunks, from
`Generate.Chunks.runtime`: `_Chunk_reg`, `_Chunk_ready`, `_Chunk_get`, and
a loader that `import()`s the file, memoizes the promise, applies the
default export to `_Chunk_scope()`, and keeps the returned record.
`_Chunk_get` returns that record, or throws `{ elmChunk: promise }`.

The marker is a plain object rather than a class so the patches can
recognise it whether or not this prelude was emitted. Its field is spelled
without leading underscores on purpose: `__name` inside a kernel file is a
token the kernel preprocessor rewrites, and the prelude -- which is not
preprocessed -- has to agree with the patches on one literal name.

**Patched into the packages**, always present and inert when unused:

- `elm/core` `Platform.js`. `_Platform_initialize`'s `sendToApp` is the
  single place `update` is called. Wrap the `update` and `subscriptions`
  calls in a `try`; on `_Chunk_Suspend`, put the message on a queue, wait
  on the promise, then drain. Messages that arrive while suspended queue
  behind it, so model updates keep their order. `init` gets the same
  treatment.
- `elm/browser` `Browser.js`. `_Browser_makeAnimator`'s `draw` is the
  single place `view` is called. Catch there, leave the dirty flag set,
  and request another frame when the chunk lands.


## What is checked, what is not

**Checked: a reference may not be forced at bundle load.** A top-level
definition runs the moment its bundle is evaluated, which is outside every
retry point.

```elm
open = Pages.Report.open       -- rejected: forced at load
open model = Pages.Report.open model   -- fine: forced when called
```

`Nitpick.AsyncImports` rejects an async reference that is not under a
lambda, working on the Canonical AST because that is where regions still
exist. It has to cover recursive definition groups too: `generateCycle`
wraps top-level cycle initialization in a `try` that swallows *any*
exception and rethrows the infinite-recursion message.

This rule will fire on ordinary point-free code, so the message has to earn
it — say that the definition would be evaluated as soon as the bundle
loads, and show the eta-expanded fix.

**Checked: no `import async` in a published package**, the way ports are
rejected. A package cannot decide its consumer's bundle layout.

**Checked: ESM output.** `.js`, `.html` and the reactor's inlined page
cannot resolve a sibling URL, so they refuse a program with async imports
rather than emitting a bundle that would fail at the first reference.

**Checked: no async import reachable from a worker program.** A worker
bundle is a separate file with its own copy of what it needs, so it has no
main bundle scope to fetch a chunk into.

**Not checked: a suspend inside a task callback.** A `Task.andThen`
callback that touches an unloaded chunk throws into `_Scheduler_step`,
which has already committed to running. In this version that is a crash.
The callbacks are pure, so making the scheduler a retry point is possible;
it is just more surface than the four entry points above.

**Not checked: `_VirtualDom_applyPatches`.** `view` and `diff` — including
forcing `Html.Lazy` thunks — both run before any DOM mutation, so a suspend
there restarts cleanly. A suspend during patch application would not. In
practice nothing is forced that late, but it is not guaranteed by
construction, and that is the one place this design rests on a property of
the current virtual-dom rather than on purity.

**Not checked: whether splitting here is a good idea.** The compiler will
happily put a chunk behind a reference on the hot path and stall the app
on a network round-trip the first time it is taken. Nothing warns about
that.


## Future extensions

- **Preload hints**: `<link rel=modulepreload>` emitted for chunks, and a
  command to warm a chunk ahead of the message that needs it — which is
  what turns a stall into a no-op for a route the user is about to open.
- **A scheduler retry point**, making task callbacks safe.
- **Lazy effect-manager setup**, so an effect module used by only one
  async page stops costing main-bundle bytes.
- **Stale chunk cleanup**: the same hashed-sibling accumulation problem
  worker bundles have.
- **Reporting**: a per-chunk byte breakdown, since "what is actually in
  this chunk" is decided by a whole-program analysis and is not obvious
  from the source.
- **Chunks inside workers**, which needs the worker bundle to grow a scope
  of its own rather than duplicating what it shares.
