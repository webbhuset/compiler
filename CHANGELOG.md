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
  listed in `"git-dependencies"` (core and browser on GitLab, virtual-dom
  on GitHub); everything else resolves from the registry as usual.

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

## Compiled pieces in elm reactor

*[docs](docs/reactor.md)*

The reactor serves a program in pieces, at the names `elm make` would have
written, so a hand-written HTML page can pull in exactly what it needs:

```html
<link rel="stylesheet" href="/src/Main.elm.css">
<script src="/src/Main.elm.js"></script>
<script type="module" src="/src/Workers/Main.elm.mjs"></script>
```

- `Main.elm.js`, `Main.elm.css` and `Main.elm.mjs` compile `Main.elm` on
  request. Your page keeps its own `<meta viewport>`, ports and flags,
  which the reactor's generated page cannot offer.
- Workers work in the reactor, from the dashboard page too: it loads a
  worker-spawning program as a module. Each spawn points at the worker
  module's own URL — `Counter.elm.mjs` compiles `Counter.elm` as a worker
  program — so there are no hashed sibling files to keep in sync, and
  compiled responses are sent `Cache-Control: no-store`.
- A worker program can be compiled on its own:
  `elm make src/Counter.elm --output=counter.mjs`.
- A failed build served as a script is a 500 whose body logs the compiler's
  report with `console.error`.

## Structural variants

*[docs](docs/structural-variants.md) · [type system](docs/types-design.md)*

Anonymous, row-polymorphic sum types — the dual of extensible records. Tags
are declared once and then combined structurally, so functions can accept
exactly the tags they handle without a shared custom type:

```elm
type tag Loading
type tag Success value

state : Int -> [ r | Loading, Success Int ]
state n =
    if n > 0 then Success n else Loading

describe : [ Loading, Success Int ] -> String
describe s =
    case s of
        Loading -> "loading"
        Success n -> "got " ++ String.fromInt n
```

- `[ A, B Int ]` is a closed row (exactly these tags); `[ r | A, B Int ]`
  is open (at least these tags), mirroring record extension syntax.
- Exhaustiveness is part of type checking: a `case` without a `_` branch
  closes the row, so an unhandled tag is a type error naming the tag.
- Row subtraction: a final catch-all variable is bound at the scrutinee row
  minus the (irrefutably) matched tags, so
  `removeLoading : r -> [ r | Loading ] -> r` works — and instantiating `r`
  derives row-changing functions like
  `[ f | Failure String, Loading ] -> [ f | Failure String ]`.
- Widening: `widen e` (a `Basics` function in the
  [patched elm/core](docs/patches/elm-core-widen.patch), identity at
  runtime) uses a variant at any row that includes its own — same tags with
  the same payloads, and a closed or identical remainder. A closed model
  field can flow into a consumer handling more tags, and a narrowed
  catch-all can be passed through into a wider result row. Checked before
  the enclosing definition generalizes; erased before code generation.
- Tags are canonical (module + name): same-spelled tags from different
  modules are distinct and can coexist in one union. Export and import
  them like constructors.
- Restrictions: tag patterns cannot sit inside tuple/list/constructor
  patterns; recursion needs a nominal wrapper type; no ports; not
  comparable; not shown in docs.json.
- Tag patterns work inside a constructor argument whose type is a type
  variable, so `Err (NotFound path)` matches straight out of a `Result`, and
  exhaustiveness still holds through the nesting. Tuples, lists, and
  constructor arguments of a fixed type stay rejected: there is no row to
  close there, so an unhandled tag would reach no branch at run time.
- Runtime: `{ $: "pkg:Module.Tag", a = ... }` in dev and prod; `==` works.

## Back-lambdas

*[docs](docs/back-lambdas.md)*

A lambda written with the arrow reversed, `\x <- ...`, binds its argument
for the lines that *follow* it, giving callback-heavy code a flat,
do-notation-like shape:

```elm
userDecoder : Decoder User
userDecoder =
    \id <- await (D.field "id" D.string)
    \firstname <- await (D.field "firstname" D.string)
    \lastname <- await (D.field "lastname" D.string)

    D.succeed { id = id, firstname = firstname, lastname = lastname }
```

- Pure syntax sugar: `\pat <- source` followed by `rest` is exactly
  `source (\pat -> rest)`. The compiler sees ordinary lambdas, so types,
  generated code, and performance are identical to writing them out.
- The continuation is the **last** argument, so `source` must take its
  callback last — define a flipped `await`/`with` helper for callback-first
  APIs like `Decode.andThen`.
- The source expression ends at the end of the line unless later lines are
  indented past the `\`; that layout rule is what keeps the continuation
  from being read as another argument.
- Several patterns bind a multi-argument callback, and any lambda pattern
  works, including destructuring.
- `\x <- ...` was previously a syntax error, so no existing program changes
  meaning — but elm-format cannot format files that use it.
- The patched elm/core adds `Task.await` (`andThen` with the task first) so
  task chains do not each need their own flipped helper.

## Command line scripts

*[docs](docs/system-scripts.md) · runtime in the `webbhuset/system` package*

A module whose `main` has this type is a program that runs on the command
line, and the compiled file runs itself — it gets a `#!/usr/bin/env node`
line and the executable bit:

```elm
main : System.Process -> Task String Int
main process =
    System.stdout ("hello " ++ String.join " " process.argv ++ "\n")
        |> Task.map (\_ -> 0)
```

```
$ elm make src/Hello.elm --output=hello.js
$ ./hello.js world
hello world
```

- The type is the contract: succeeding with an `Int` exits with that
  status, failing with a `String` prints it to stderr and exits 1. No
  separate exit API is needed for the normal path.
- `Process` carries `argv` (without the node binary and script path), an
  `env` dict, and `platform`. Things that change while the program runs,
  like the working directory, are tasks instead of fields.
- `System` has stdout/stderr/stdin, `isTerminal`, `cwd`/`chdir` and
  `exit`; `System.File` has the usual file and directory operations;
  `System.Path` joins and takes apart paths the way the platform expects;
  and `System.Child` runs other programs, capturing their output or
  letting it through to the terminal.
- Failures are structural variant tags, so each operation says what it can
  actually fail with, chaining unions the rows, and handling one tag with
  a catch-all removes it from what the caller sees. An error code with no
  tag crashes, naming the code and asking for a report, so gaps in the
  vocabulary get found rather than hidden behind a catch-all.
- A script must be the only program compiled, `--output` must be `.js` or
  `.mjs`, and the DEV mode console warning is suppressed since a program's
  stderr is part of its contract.
- Long running programs that must react to events (watchers, servers,
  signals) want a message loop instead: write those as a `Platform.worker`
  with ports. The `System.*` tasks work there unchanged.

## Overloading by signature

*[docs](docs/overloading.md)*

One name can have many definitions, and the compiler picks the one whose
signature matches the type it is used at. A module declares a name with
`abstract`; other modules define it by writing that name qualified, with a
concrete signature and a body:

```elm
module Ord exposing (Ordering(..))

abstract compare : a -> a -> Ordering
```

```elm
module Card exposing (Card(..))

Ord.compare : Card -> Card -> Ordering      -- a definition
Ord.compare (Card a) (Card b) =
    ...
```

- No `class` and no `instance`: one keyword and a qualified name in
  definition position is the whole ceremony, and the use site reads like
  any other qualified call. `abstract` is contextual, like `port`, so a
  value of that name still works.
- The first argument decides which definition a use site means, so an
  abstract signature has to start with a type variable and a definition
  has to start with a named type.
- A definition must live in the module that declares the name, or in the
  module that declares the type it dispatches on. That gives exactly one
  definition per (name, type) pair in any program, with no orphan rules.
- Resolution happens after type inference, so it dispatches on the type
  the solver settled on. There is no dictionary and no runtime dispatch:
  each use site becomes a direct call, and unused definitions are dead
  code like any other.
- A signature says which overloads it needs on its own type variables, so
  that a name can be used where the dispatch type is not yet known:

  ```elm
  smallest : a -> a -> Ordering
      where Ord.compare : a -> a -> Ordering
  ```

  Each clause becomes a hidden leading parameter, and nothing about it
  shows in the type. Definitions can be constrained too, so
  `Ord.compare : List a -> List a -> Ordering where Ord.compare : a -> a
  -> Ordering` works and a use at `List (List Card)` builds what it needs
  recursively.
- An operator dispatches when the function behind it does, so
  `infix non 4 (|<|) = lt` on a constrained `lt` gives an overloaded
  operator. `<` and friends still belong to `Basics`, which every module
  imports openly, so only elm/core can give those a new meaning.
- A definition can be for a tuple, which is what `comparable` covers that
  a named type does not, or for a closed row carrying one structural
  variant tag, since a tag's identity is already its module plus its name.
  Dispatch sees through type aliases, so `type alias Name = String` cannot
  carry a second definition for String. `comparable` itself cannot be a definition: it is
  a type variable, so it would be a default overlapping every real one.
- An overload used on a type variable with no clause for it reports the
  exact line to add. Clauses are not inferred, only suggested, and a `let`
  definition cannot have them yet. `comparable` and friends are untouched.

## HTML to string

*[docs](docs/html-to-string.md) · runtime in a
[patched elm/virtual-dom](docs/patches/elm-virtual-dom-to-string.patch)*

`VirtualDom.toString` renders a node as HTML text, for serving a page from a
server instead of building it in a browser. The `Int` is the indentation
width, where `0` adds no whitespace at all — the only setting that cannot
change what the page means:

```elm
V.toString 0 (Html.p [] [ Html.text "Hello!" ])
--> "<p>Hello!</p>"
```

Two node kinds go with it, `V.comment` and `V.doctype`, so a whole document
can be written from Elm. A comment is a real comment node in a browser and
diffs like any other node; `virtualize` keeps the comments in
server-rendered markup, so an app taking over a pre-rendered page sees them
in place. A doctype has no DOM node it could be and renders as an empty
text node there.

The output is the tree as written: a `script` tag stays a script tag and an
`on*` attribute keeps its name. Those two rewrites are defenses against
injecting into *this* document, so they moved from where a node is built to
`_VirtualDom_render` and `_VirtualDom_applyAttrs`. The browser is defended
exactly as before, but an attribute name built from user input now reaches
your server output, where it used to be neutralized for you. Text and
attribute values are escaped. `Html.Attributes.href`, `src` and `action`
still refuse a `javascript:` URI, since elm/html checks that where the
attribute is built.

Event handlers, custom nodes and `innerHTML` cannot be written down and are
left out. Properties are translated to attributes (`className` to `class`,
`htmlFor` to `for`, booleans to HTML boolean attributes).

## Compatibility notes

- **elm.json**: the only addition is the optional `"git-dependencies"`
  field, which official parsers ignore.
- **elm/core**: task ports, comparable newtypes, and `Task.await` need a
  patched elm/core, `Css.vars` needs a patched elm/virtual-dom, and web workers
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
- **Object files**: task ports add a node kind, CSS blocks and web workers
  each add an expression kind, and command line scripts add a main kind to
  the `.elmo` format; stale `elm-stuff` from other compilers is detected
  and rebuilt.
- **Interfaces**: overloading adds a per-module table of abstract names and
  definitions to the interface format, which is what makes a definition in
  one module reachable from a use site in another.

## Cross-platform release binaries

Pushing a `v*` tag builds native binaries for `linux-x64`, `linux-arm64`,
`darwin-arm64`, and `win32-x64` via `.github/workflows/release.yml` and
attaches them (gzip / zip) to a **draft** GitHub Release for review before
publishing. Linux binaries are fully static (built on Alpine/musl); each binary
is smoke-tested with `elm --version` in CI before it is uploaded. (Intel macOS,
`darwin-x64`, is not built: GitHub retired the `macos-13` runner image on
2025-12-04, and it is the last hosted Intel image. Only Apple Silicon macOS,
`darwin-arm64`, is produced.)
