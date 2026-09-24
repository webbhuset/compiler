# Changes from upstream

This is webbhuset's fork of the [Elm compiler](https://github.com/elm/compiler).
It tracks upstream `main` (0.19.2) and adds the features below. Projects
that use none of these features compile exactly as with the official
compiler, and every `elm.json` this fork writes remains valid for official
tooling (elm-format, elm-test, editors).


## Contents

- **[Bugfix, performance and quality of life](#bugfix-performance-and-quality-of-life)**
  - [Exponential compile time and memory with extensible records](#exponential-compile-time-and-memory-with-extensible-records)
  - [Git dependencies — private packages](#git-dependencies--private-packages)
  - [Kernel code in git dependencies](#kernel-code-in-git-dependencies)
  - [ES module output](#es-module-output)
  - [Compiled pieces in elm reactor](#compiled-pieces-in-elm-reactor)
  - [Direct function calls](#direct-function-calls)
  - [Record update by spread](#record-update-by-spread)
  - [Tail recursion modulo cons](#tail-recursion-modulo-cons)
- **[Native and compiler output](#native-and-compiler-output)**
  - [Command line scripts](#command-line-scripts)
  - [Task ports](#task-ports)
  - [Native web workers](#native-web-workers)
  - [HTML to string](#html-to-string)
  - [HTTP over fetch](#http-over-fetch)
  - [Code splitting — async imports](#code-splitting--async-imports)
- **[New language features](#new-language-features)**
  - [Comparable newtypes](#comparable-newtypes)
  - [CSS blocks](#css-blocks)
  - [Overloading by signature](#overloading-by-signature)
  - [Back-lambdas](#back-lambdas)
  - [Structural variants](#structural-variants)
- **[Compatibility notes](#compatibility-notes)**
  - [No `elm publish`](#no-elm-publish)
  - [Cross-platform release binaries](#cross-platform-release-binaries)


# Bugfix, performance and quality of life

Bugfixes, faster generated code, or replacing external tools. No change
to the language.

## Exponential compile time and memory with extensible records

*fixes [elm/compiler#1897](https://github.com/elm/compiler/issues/1897)*

Nesting extensible-record aliases doubled both compile time and memory for
every level of nesting, so a chain of them became unbuildable long before
it became unreadable:

```elm
type alias Part1 a = { a | part1 : String }
type alias Part2 a = { a | part2 : String }
type alias Part3 a = { a | part3 : String }


type alias Parts =
    Part1 (Part2 (Part3 {}))
```

- With 30 nested parts, compiling goes from 23 s and 27 GB to 0.03 s and
  57 MB.
- Two places converted an alias's arguments once per level.
  `Type.Instantiate.fromSrcType` substituted the instantiated arguments
  into `Holey` alias bodies, so a separate variable graph was built for
  the copy in the body and the copy in the argument list; it now leaves a
  `PlaceHolder`, which `Solve.typeToVar` already resolves to the shared
  argument variable.
- `Type.toAnnotation` and `toErrorType` converted an alias's arguments and
  its real type independently, and since each record's extension runs
  through the next alias, everything below was converted again at every
  level. Variables equivalent to an alias argument are now mapped back to
  it, so inferred annotations emit `Can.Holey` bodies the way
  canonicalization already did for written ones.
- Documentation output and error messages are unchanged.


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
- Cloning a repository can take a while, and git's output is captured, so
  each package is announced with its URL before it is fetched:
  `↓ cloning elm/core 1.100.504 (git@github.com:webbhuset/core.git)`. Nothing
  is printed for a package already in the cache, or under `--report=json`.
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
- One module per program. Several programs in one invocation are split:
  each one's code goes in a content-hashed sibling fetched when it is
  started, and the module keeps what they share (see code splitting
  below). A program can ask for more files too: `import async` writes one
  chunk per async-imported module and `Worker.spawn` one bundle per worker
  program. All of these resolve their files against `import.meta.url`,
  which is why they need this mode.

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
- Async imports work in the reactor too, and a program that has them is
  loaded as a module for the same reason a worker-spawning one is. A
  chunk is not a program, so it cannot be served from its own source
  file; it comes from the root program's endpoint instead, as
  `/src/Main.elm.mjs?chunk=Pages.Report`.
- A failed build served as a script is a 500 whose body logs the compiler's
  report with `console.error`.


## Direct function calls

Upstream wraps every function of two to nine parameters in `F2`..`F9` and
routes every call through `A2`..`A9`, which checks the arity at run time
and either calls the underlying function or applies the arguments one at a
time. This fork emits each such top-level function twice — the bare
JavaScript function under a `$fn$` name, and the wrapped one under the
usual name — and calls the bare one directly wherever the arity is known
to match:

```js
var $author$project$Lib$fn$scale = function (k, shape) { ... };
var $author$project$Lib$scale = F2($author$project$Lib$fn$scale);

$author$project$Lib$fn$scale(2, shape)          // was A2($author$project$Lib$scale, 2, shape)
$author$project$Lib$fn$adder(1, 2)(3)           // was A3($author$project$Lib$adder, 1, 2, 3)
{$: 1, a: k * w, b: k * h}                      // was A2($author$project$Lib$Rect, k * w, k * h)
_List_Cons(x, xs)                               // was A2($elm$core$List$cons, x, xs)
```

- Applies to top-level functions in every package, tail-recursive
  functions, functions in mutually recursive groups, constructors and
  structural variant tags. A call with more arguments than parameters
  calls the bare function and applies the rest to its result.
- Partial application, higher-order use and kernel code keep going through
  the wrapped name, so nothing observable changes; it is the same idea as
  the "applying functions directly" transformation in
  [elm-optimize-level-2](https://github.com/mdgriffith/elm-optimize-level-2/blob/master/notes/transformations.md),
  done where the arity of every definition is already known.
- The compiler knows the arity from the optimized definition, not from the
  type, so a function defined as `f a = \b -> ...` is called with one
  argument and the result applied to the next.
- This trades size for speed: every function of two to nine parameters
  gains a second definition, and a small program grew by about three
  percent after minification and gzip. The saving from dropped `A2(`
  wrappers only offsets that in code with many saturated calls.
- Code-splitting chunks receive the `$fn$` names they call through the
  same scope object as everything else.

## Record update by spread

`{ rec | count = rec.count + n }` compiles to an object spread instead of
a call to the kernel's `_Utils_update`, which rebuilt the record one
property at a time in two `for...in` loops:

```js
{...rec, count: rec.count + n}          // was _Utils_update(rec, {count: rec.count + n})
```

- The spread copies the record's shape in one step and skips the
  temporary object of updated fields. The result has the same properties
  in the same order as before.
- Object spread is ES2018 syntax. This fork's output already assumes
  modern JavaScript, so it applies to every output mode, `.js` included.

## Tail recursion modulo cons

Upstream turns a self call in tail position into a loop. This fork also
does it when the only thing left after the call is to put its result in a
constructor:

```elm
map f list =
    case list of
        [] -> []
        x :: xs -> f x :: map f xs
```

compiles to a loop like this one (temporaries elided) that builds the list
front to back, so it neither grows the stack nor allocates a frame per
element:

```js
var $start = {b: null};
var $end = $start;
map:
while (true) {
    if (!list.b) {
        $end.b = _List_Nil;
        return $start.b;
    } else {
        var x = list.a, xs = list.b;
        var $cell = _List_Cons(f(x), _List_Nil);
        $end.b = $cell;
        $end = $cell;
        list = xs;
        continue map;
    }
}
```

- Applies to `::` and to any saturated constructor with the self call as
  exactly one of its arguments, in top-level and `let` functions alike.
  Plain self tail calls in the same function keep working; they append
  nothing.
- Every site in a function has to fill the same constructor field. A
  function that recurses through two different fields, or through two
  cells at once (`x :: y :: recurse xs`), is compiled as before.
- The result is built by mutating the last cell, which nothing else can
  observe: the list does not exist until the function returns it.
- Arguments after the hole are evaluated before the recursion instead of
  after it. Elm is pure, so only a `Debug.log` in such an argument can tell.

# Native and compiler output

Changes to the compiler and to some kernel packages, but not new language
features. Elm itself stays the same; each of these could be replaced by
boilerplate and external tools.

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

## HTTP over fetch

*fork: `webbhuset/elm-http` 2.100.1, opt-in through
[git dependencies](docs/git-dependencies.md)*

`elm/http` is built on `XMLHttpRequest`, which exists only on a browser's
main thread. That kept `Http` out of exactly the places this fork otherwise
opens up — command line scripts, web workers, service workers. The forked
package does the same job with `fetch`, so the module works unchanged in
all of them:

```json
"dependencies": {
    "direct": { "elm/http": "2.100.1", ... }
},
"git-dependencies": {
    "elm/http": "git@github.com:webbhuset/elm-http.git"
}
```

- The public API is untouched. `Http.get`, `Http.request`, `Http.track`,
  `expectString`/`expectBytes`/`expectJson`, `riskyRequest`, timeouts and
  cancellation all behave as before, and the four `Http.Error` cases still
  mean what they meant.
- An `AbortController` backs cancellation, and a flag set before the abort
  keeps `Timeout` distinguishable from a cancel — `fetch` reports both the
  same way, as it does a bad URL and a dead network, so a bad URL is caught
  while the `Request` is constructed instead.
- Download progress reads the response stream; upload progress needs a
  request stream, which Firefox and Safari do not have. Uploads still work
  there, but `Http.Sending` never fires.
- Two deliberate differences from XHR: a body's declared mime now
  overwrites an explicit `Content-Type` header rather than being combined
  with it, and a body on `GET` or `HEAD` is dropped rather than throwing,
  so code that worked before does not become a `BadUrl`.
- Not pinned by `elm init`, since not every project wants HTTP. Name it in
  `"git-dependencies"` to opt in.

## Code splitting — async imports

*[docs](docs/code-splitting.md) · design notes:
[docs](docs/code-splitting-design.md) · runtime: patches to
[elm/core](docs/patches/elm-core-code-splitting.patch) and
[elm/browser](docs/patches/elm-browser-code-splitting.patch) ·
requires `--output=something.mjs`*

Everything reachable from `main` is downloaded and evaluated before the
program starts, including the screen nobody opens. Mark an import `async`
and that module's code moves into a file of its own, fetched the first
time something needs it:

```elm
import async Pages.Report

view : Model -> Html Msg
view model =
    case model of
        Reporting n ->
            -- the file is fetched here, the first time
            Pages.Report.open n

        Counting n ->
            viewCounter n
```

`import async M` brings exactly the same names into scope as `import M`,
at the same types, used at the same call sites — `as` and `exposing` work
as usual, and `async` remains a legal variable name.

- `elm make src/Main.elm --output=app.mjs` writes `app.mjs` plus one
  content-hashed `app.<hash>.mjs` per async-imported module.
- There is nothing to await at the call site, because the value is an
  ordinary Elm value, not a promise. A reference to a file that has not
  arrived throws a marker carrying it, and the four places the runtime
  calls into user code — `init`, `update`, `view`, `subscriptions` —
  catch it, wait, and call again. That is safe because Elm is pure:
  nothing the first call produced was kept. While a file is in flight the
  app pauses rather than showing a partial state, and messages that arrive
  meanwhile are handled in order once it lands.
- A chunk takes whatever only it needs, transitively, packages included —
  a chart library used by one screen leaves the initial download
  entirely. Code a second chunk also needs is hoisted into the main
  bundle, so one page never holds two copies of a value. Effect managers
  and kernel code stay in the main bundle as well, since they are
  registered when the program starts.
- A module that is reachable without the async import anyway is already
  in the main bundle, so the import costs nothing and fetches nothing.
- Several applications compiled into one `.mjs` are split the same way
  with no `import async` at all: one chunk per application, fetched the
  first time it is started, and the shared code in the module itself.
  `Elm.App1.init` still returns at once, with the application's ports on
  the object; what the page sends or subscribes before the chunk lands is
  replayed once the program starts. An application reachable from another
  one anyway stays in the module.
- `--optimize` works normally: one `elm make` means one field-rename
  table, so values cross between the files unchanged.
- Compile errors for a reference that would be forced when the bundle
  loads (a top-level definition written without arguments), for
  `import async` in a package, for non-ESM output, and for an async
  import reached from a worker program.
- Not a retry point: a chunk first touched inside a `Task` callback
  raises a JavaScript error naming the problem rather than waiting, since
  the scheduler has already committed to running it.


# New language features

Changes to the syntax or the type system, exploring what Elm could look
like. These would be hard to do outside the compiler.

## Comparable newtypes

*[docs](docs/comparable-newtypes.md) · requires a
[patched elm/core](docs/patches/elm-core-comparable-newtypes.patch)*

Custom types with exactly one constructor wrapping exactly one comparable
value satisfy `comparable`, so they work as `Dict` keys, in `Set`s, with
`List.sort`, `compare`, and friends:

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
- Type parameters follow the `List a` rule: `type Box a = Box a` is
  comparable exactly when `a` is, and a phantom parameter the payload never
  mentions does not matter, so `type Id t = Id String` is comparable for
  every `t`.
- Multi-constructor types, records, and functions are unchanged: still not
  comparable.
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


# Compatibility notes

- **elm.json**: the only addition is the optional `"git-dependencies"`
  field, which official parsers ignore.
- **elm/core**: task ports, comparable newtypes, and `Task.await` need a
  patched elm/core, `Css.vars` needs a patched elm/virtual-dom, web workers
  need a patched elm/browser, async imports need both elm/core and
  elm/browser, and `Http` off the browser main thread needs a patched
  elm/http (all patches are in
  [docs/patches/](docs/patches/)), consumed through git dependencies
  under unpublished version numbers. The elm/core patches
  are additive; programs not using the features behave identically. The
  virtual-dom patch applies styles with `setProperty`, which only accepts
  hyphenated CSS names — camelCase keys like `style "backgroundColor"`
  (already against elm/html convention) stop working.
- **Caches**: this fork keeps its caches in directories of its own —
  `$ELM_HOME/webbhuset-0.19.2/` for packages (so `~/.elm/webbhuset-0.19.2/`
  by default) and `elm-stuff/webbhuset-0.19.2/` inside a project — rather
  than the `0.19.2/` the version number alone would give. The interface and
  object file formats carry extra information the official compiler cannot
  read, so this lets the two share an `ELM_HOME` and a project directory
  without invalidating each other's caches, at the cost of downloading each
  package once per compiler. `ELM_HOME` itself still selects the root, so an
  existing override keeps working, and the fork's build artifacts still live
  under `elm-stuff/`, which `.gitignore` files already cover.
- **Object files**: task ports add a node kind, CSS blocks, web workers,
  async imports and tail recursion modulo cons each add an expression kind,
  and command line scripts add a main kind to the `.elmo` format; stale `elm-stuff` from other compilers is
  detected and rebuilt. Moving between *builds of this fork* that changed
  the format is noisier: the package artifacts in `ELM_HOME` are reported
  as corrupt before being rebuilt.
  `find ~/.elm/webbhuset-* -name artifacts.dat -delete` once, after
  upgrading, avoids the warning.
- **Interfaces**: overloading adds a per-module table of abstract names and
  definitions to the interface format, which is what makes a definition in
  one module reachable from a use site in another.

## No `elm publish`

The `publish` command is removed from this fork's CLI, so nothing built here
can reach <https://package.elm-lang.org> by accident. A package using any
fork-only feature would be unusable to anyone on the official compiler, and
the mistake is not undoable once a version is published. `elm bump` and
`elm diff` are untouched, and a package meant for the official registry is
published with the official compiler.

## Cross-platform release binaries

Pushing a `v*` tag builds native binaries for `linux-x64`, `linux-arm64`,
`darwin-arm64`, and `win32-x64` via `.github/workflows/release.yml` and
attaches them (gzip / zip) to a **draft** GitHub Release for review before
publishing. Linux binaries are fully static (built on Alpine/musl); each binary
is smoke-tested with `elm --version` in CI before it is uploaded. (Intel macOS,
`darwin-x64`, is not built: GitHub retired the `macos-13` runner image on
2025-12-04, and it is the last hosted Intel image. Only Apple Silicon macOS,
`darwin-arm64`, is produced.)
