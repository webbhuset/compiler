# Changes from upstream

This is webbhuset's fork of the [Elm compiler](https://github.com/elm/compiler).
It tracks upstream `main` (0.19.2) and adds the features below. Projects
that use none of these features compile exactly as with the official
compiler, and every `elm.json` this fork writes remains valid for official
tooling (elm-format, elm-test, editors).

## Git dependencies — private packages

*[docs](docs/git-dependencies.md)*

Packages can be fetched directly from git repositories instead of the
official registry, via a new top-level `elm.json` field. The package is
still listed in the normal dependency fields; versions are git tags named
like Elm versions (`1.2.0`):

```json
"dependencies": {
    "direct": { "webbhuset/elm-promise": "1.2.0", ... }
},
"git-dependencies": {
    "webbhuset/elm-promise": "git@github.com:webbhuset/elm-promise.git"
}
```

- Authentication is git's problem: SSH agents and credential helpers work
  as usual. Any URL `git clone` accepts works, including local paths.
- Sources are shallow-cloned once into the shared `ELM_HOME` package
  cache. A `git-url` file records the origin; mapping the same package
  name and version to a different URL is an error, not a silent reuse.
- For applications no network is needed to discover versions (they are
  pinned in `elm.json`); package projects and `elm install` discover
  versions with `git ls-remote --tags`.
- `elm install` works for git dependencies and preserves the field when
  rewriting `elm.json`. The field is never written unless present.
- `elm publish` rejects packages that have git dependencies, since their
  dependencies are not publicly resolvable.
- A name+version is expected to be immutable: if you move a tag, delete
  the package's directory from `ELM_HOME` to force a fresh clone. The same
  applies when the *URL spelling* for a name+version changes (e.g. a local
  path vs. the GitLab URL): equivalent spellings count as different
  origins.
- `elm init` writes a project that starts on the patched forks: elm/core,
  elm/browser, and elm/virtual-dom are pinned to the fork versions and
  listed in `"git-dependencies"` pointing at the GitLab repositories;
  everything else resolves from the registry as usual.

## Kernel code in git dependencies

*[docs](docs/git-dependencies.md)*

Packages fetched through `git-dependencies` are trusted like the `elm/*`
packages: they may define `Elm.Kernel.*` JavaScript modules, effect
managers, and custom infix operators. Intended for private packages that
need native code, e.g. server modules running on Node.js.

- Kernel module short names are global across all packages: prefix yours
  (e.g. `Elm.Kernel.WhServer`) to avoid colliding with `elm/*`.
- This also makes it possible to override `elm/core` itself with a
  patched fork under an unpublished version number, which is how the two
  features below ship their runtime parts.
- Kernel JavaScript only takes effect in packages consumed from the
  package cache; `elm make` inside the kernel package itself does not
  include it (an upstream limitation the elm organization also lives
  with). Develop against a test application.

## Task ports

*[docs](docs/task-ports.md) · requires a
[patched elm/core](docs/patches/elm-core-task-ports.patch)*

Ports can produce a `Task`, backed by a promise-returning JavaScript
function — request/response FFI without a `Cmd`/`Sub` port pair:

```elm
port fetchUser : { id : String } -> Task Json.Decode.Value { name : String }
```

```js
Elm.Main.init({
    taskPorts: {
        fetchUser: async (args) => (await fetch("/api/users/" + args.id)).json()
    }
});
```

- Composes like any task: `Task.andThen`, `Task.map`, `Task.sequence`,
  run with `Task.attempt` / `Task.perform`.
- Argument encoders and result decoders are derived by the compiler
  exactly as for `Cmd`/`Sub` ports; the same payload type rules apply.
- The error type is fixed to `Json.Decode.Value` (JavaScript can reject
  with anything). Rejections, thrown exceptions, results that fail the
  decoder, and missing implementations all fail the task with a
  descriptive `Error` value.
- Implementations are registered in `init`, so they exist before the
  program's `init` commands run. The registration table is shared per
  compiled bundle; the most recent `init` wins for shared names.
- Cancellation is not supported; a killed process drops the result but
  does not abort the promise.

## ES module output

*[docs](docs/esm-output.md)*

Naming the output `.mjs` produces an ES module instead of the classic
IIFE that assigns `window.Elm`:

```
elm make src/Main.elm src/Pages/Home.elm --output=elm.mjs
```

```js
import { Elm } from "./elm.mjs";   // also the default export
Elm.Main.init({ node: ... });
```

- Same `Elm` object shape as upstream, including nested module names and
  multiple mains in one file. Works in browsers, Node.js, and bundlers.
- Nothing is assigned to the global scope, and separate `.mjs` bundles do
  not merge into a shared `Elm` object the way classic bundles do.
- `--output=foo.js` and `--output=foo.html` are byte-for-byte unchanged.
- Always a single module; code splitting is left to bundlers.

## Comparable newtypes

*[docs](docs/comparable-newtypes.md) · requires a
[patched elm/core](docs/patches/elm-core-comparable-newtypes.patch)*

Custom types with exactly one constructor wrapping exactly one concrete
comparable value satisfy `comparable`, so they work as `Dict` keys, in
`Set`s, with `List.sort`, `compare`, and friends:

```elm
type Id
    = Id String


users : Dict Id User
```

- Payloads may be `Int`, `Float`, `Char`, `String`, lists and tuples of
  comparables, and other comparable newtypes — transitively, across
  modules and packages. Works for opaque types (unexported constructors).
- Ordering is the payload's ordering. In `--optimize` builds these types
  are already unboxed, so comparison is unchanged there; dev builds
  unwrap at runtime (the elm/core patch).
- Multi-constructor types, records, functions, and parameterized types
  (`type Box a = Box a`) are unchanged: still not comparable.
- `elm diff` does not detect that changing a payload to something
  non-comparable breaks downstream `Dict` users; treat it as a major
  change yourself.

## CSS blocks

*[docs](docs/css-blocks.md) · runtime in the `webbhuset/css`
package · `Css.vars` requires a
[patched elm/virtual-dom](docs/patches/elm-virtual-dom-custom-properties.patch)*

CSS is embedded the way GLSL shaders are: the compiler parses a
`[css| ... |]` block and infers a record type from it, so HTML and CSS can
no longer drift apart — removing or renaming a class in the CSS turns every
use site into an ordinary type error:

```elm
sheet =
    [css|
        @property --progress { syntax: "<percentage>"; inherits: false; }

        .bar {
            width: var(--progress);
            transition: width 0.2s;
        }
    |]

-- inferred:
-- sheet : Css.Stylesheet { bar : Css.Class } { progress : Css.Percentage }


view model =
    let
        c = Css.classes sheet
    in
    div
        (Css.class c.bar :: Css.vars sheet { progress = Css.pct model.progress })
        []
```

- Class selectors become `Css.Class` fields and `@keyframes` names become
  `Css.Animation` fields. Custom properties that a block consumes but never
  assigns become required inputs, supplied per element with `Css.vars` and
  typed by their `@property` syntax descriptor (`"<percentage>"` gives
  `Css.Percentage`, and so on; untyped inputs are `Css.Value`).
- Identifiers in `animation`/`animation-name` must be keywords or declared
  `@keyframes` — a misspelled animation name, a silent no-op in browsers,
  is a compile error.
- Emitted names are module-scoped (`Page-Checkout--card`), giving
  CSS-modules-style local scoping; `--optimize` shortens them to one or
  two characters. Class names must be lowerCamelCase, since they become
  record fields.
- The CSS text is never in the JS bundle, which holds only name
  translation tables. `--output=bundle.mjs` (or `.js`) writes a `bundle.mjs.css`
  sidecar containing exactly the blocks that survive dead-code
  elimination; `.html` output and `elm reactor` inline a `<style>`.
- The consuming side (`Css.classes`, `Css.class`, `Css.vars`, value
  constructors like `px`/`pct`/`rgb`) lives in the `webbhuset/css` package,
  consumed as a git dependency since it has kernel code. External CSS can
  still be referenced explicitly, e.g.
  `Css.value "var(--brand-color)"` for a page-level design token.

## Native web workers

*[docs](docs/web-workers.md) · runtime: `Browser.Worker` in a
[patched elm/browser](docs/patches/elm-browser-worker.patch) ·
requires `--output=something.mjs`*

A worker is an Elm module whose `main` is a `Worker.Program`, compiled into
its own JavaScript file by the same `elm make` that compiles the app
spawning it. Because both sides share one compilation (and one `--optimize`
rename table), messages cross the boundary as ordinary Elm values via
structured clone — custom types included, no JSON encoders to drift:

```elm
-- Counter.elm
main : Worker.Program Args ToParent Msg Model
main =
    Worker.worker { init = init, update = update, subscriptions = subscriptions }


-- Main.elm
Worker.spawn Counter.main
    { initial = 10 }
    { onSpawn = GotCounter, onMessage = FromCounter, onCrash = CounterCrashed }
```

- `elm make src/Main.elm --output=main.mjs` writes `main.mjs` plus one
  content-hashed `main.<hash>.mjs` per spawned worker. Workers can spawn
  workers; only the workers reachable from `main` are emitted.
- The worker's `init` receives the spawner's `Channel` for messages upward;
  the spawner gets a `Worker` handle (send + `kill`). A worker can `stop`
  itself; a channel can only send, so a worker cannot kill its parent.
- Subscriptions, tasks, and effect managers work normally inside workers —
  anything that does not need the DOM. Timers in workers keep running while
  the page tab is hidden.
- The spawned program must be a direct reference to a top-level value
  (`Worker.spawn Counter.main args handlers`); anything else is a compile
  error, as is compiling a worker-spawning program to `.js`/`.html`.
- Messages must be function-free (structured clone); violations fail at
  runtime via `onCrash`. CSS blocks inside worker code land in the same
  `.css` sidecar as the rest of the program.
- The `Browser.Worker` module ships in a patched `elm/browser` (kernel
  code plus an effect manager), consumed as a git dependency. No elm/core
  or virtual-dom patches needed.

## Compatibility notes

- **elm.json**: the only addition is the optional `"git-dependencies"`
  field, which official parsers ignore.
- **elm/core**: task ports and comparable newtypes need a patched
  elm/core, `Css.vars` needs a patched elm/virtual-dom, and web workers
  need a patched elm/browser (all patches are in
  [docs/patches/](docs/patches/)), consumed through git dependencies
  under unpublished version numbers. The elm/core patches
  are additive; programs not using the features behave identically. The
  virtual-dom patch applies styles with `setProperty`, which only accepts
  hyphenated CSS names — camelCase keys like `style "backgroundColor"`
  (already against elm/html convention) stop working.
- **Caches**: the interface file format carries extra information, so the
  first build with this fork rebuilds `elm-stuff` and the `ELM_HOME`
  package artifacts automatically. Do not alternate this fork and the
  official compiler on the same `ELM_HOME` — they will repeatedly
  invalidate each other's caches.
- **Object files**: task ports add a node kind, and CSS blocks and web
  workers each add an expression kind to the `.elmo` format; stale
  `elm-stuff` from other compilers is detected and rebuilt.
