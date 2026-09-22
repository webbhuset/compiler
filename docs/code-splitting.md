# Code Splitting

Mark an import `async` and that module's code moves out of the main bundle
into a file of its own, fetched the first time something needs it.

```elm
import async Pages.Report


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenReport ->
            ( Reporting, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Counting n ->
            viewCounter n

        Reporting ->
            -- the file is fetched here, the first time
            Pages.Report.open
```

```
elm make src/Main.elm --optimize --output=app.mjs
```

writes `app.mjs` plus one `app.<hash>.mjs` per async-imported module.
Nothing else changes: `import async M` brings exactly the same names into
scope as `import M`, the types are the same, and the call sites are the
same. Requires `--output=*.mjs` (see below).


## How the wait works

There is nothing to await at the call site, because `Pages.Report.open` is
an ordinary `Html msg`, not a promise. Instead the runtime catches the
first reference to a chunk that has not arrived, waits for it, and runs the
same pure function again -- your `update`, `view`, `init` or
`subscriptions` -- as if it had been called a moment later. Nothing it did
the first time was kept: no model assigned, no commands run, no DOM
touched.

So while a chunk is in flight the app pauses rather than showing a partial
state, and messages that arrive meanwhile are handled in order once it
lands. If you want a spinner, load the chunk from a message that also sets
a "loading" flag, rather than expecting one for free.


## What goes in a chunk

Whatever only that module needs, transitively -- including packages the
rest of the program never touches, which is usually where the win is. A
chart library used by one screen leaves the initial download entirely.

What does not move:

- **Code a second chunk also needs** stays in the main bundle, so there is
  never a second copy of a value in the same page.
- **Effect managers** stay in the main bundle. They are registered when
  the program starts, so one arriving later could never receive a command.
  An `effect module` used only by an async page still costs its bytes up
  front.
- **A module that is reachable without the async import anyway** is
  already in the main bundle. The import then costs nothing and fetches
  nothing -- it does not silently make a second copy.

`--optimize` works normally. Both files are produced by one `elm make`, so
they share one field-rename table and values cross between them unchanged.


## Several applications in one file

Compiling more than one program into one ES module splits it the same way,
with no `import async` anywhere: each application's code goes in a chunk of
its own, fetched the first time that application is started, and the module
itself keeps what the applications share -- `elm/core`, the virtual DOM,
effect managers, and any of your modules more than one of them imports.

```
elm make src/App1.elm src/App2.elm --output=bundle.mjs
```

writes `bundle.mjs` plus one `bundle.<hash>.mjs` per application.

```js
import { Elm } from "./bundle.mjs";

const app1 = Elm.App1.init({ flags: 21 });   // fetches App1's chunk
app1.ports.nameIn.send("World");             // fine: queued until it lands

const app2 = Elm.App2.init({});              // fetches App2's chunk
```

`init` returns at once, as it always has, and the object it returns carries
the ports that application can reach. Until the chunk arrives, `send`
queues its value and `subscribe` remembers its callback; both are replayed
against the real program the moment it starts, in the order they happened.
Messages an outgoing port sends from `init` are not lost either: Elm
delivers them after a `sleep 0` in any case, so a subscriber registered
right after `init` still sees them. `node`, `flags` and `taskPorts` are
passed through untouched, so a `Browser` program renders into its node when
its chunk lands.

A page that starts one application now downloads two files instead of one,
the shared module and that application's chunk, and the sibling files have
to be deployed next to the module. An application that is reachable from
another one anyway -- `App1` imports `App2` and touches its `main` -- is
already in the shared module, and its `init` is the ordinary one.

Nothing changes for a single program, for `.js` output, or for the reactor,
which compiles one program at a time. There is no way to turn the split off
yet; a `--split` flag is a likely follow-up.


## Rules

**Use it under a lambda.** A top-level definition with no arguments is
evaluated the moment the bundle loads, before the program exists, so there
is nothing to run again:

```elm
open = Pages.Report.open         -- rejected
open arg = Pages.Report.open arg -- fine
```

The compiler rejects the first with an error saying so.

**`--output` must be `.mjs`.** Chunk files are loaded relative to the
bundle that names them, and only an ES module can know its own URL (via
`import.meta`). `.js`, `.html`, and the reactor's inlined page cannot, and
report an error. See `esm-output.md`.

**Packages cannot use it.** How a program is split is the application's
decision; a package exposes its values normally and lets the application
choose.

**Not inside a web worker.** A worker bundle is a separate file with its
own copy of what it needs, so there is nowhere to fetch a chunk into.
Import the module normally in the code a worker uses; the page can still
import it with `async`.

**Not from a task callback.** A `Task.andThen` callback that is the very
first thing to touch a chunk crashes rather than waiting -- the scheduler
has already committed to running it by then. Reach the chunk from `update`
or `view` first, which is the normal shape anyway.


## Runtime: patched elm/core and elm/browser

The waiting happens in kernel code, so both packages are forks, consumed as
[git dependencies](git-dependencies.md) under unpublished version numbers.
`elm init` pins these already:

```json
"dependencies": {
    "direct": { "elm/core": "1.100.504", "elm/browser": "1.100.202", ... }
},
"git-dependencies": {
    "elm/core": "git@github.com:webbhuset/core.git",
    "elm/browser": "git@github.com:webbhuset/elm-browser.git"
}
```

elm/core catches the marker where it calls `init`, `update` and
`subscriptions` ([patches/elm-core-code-splitting.patch](patches/elm-core-code-splitting.patch));
elm/browser catches it around the view
([patches/elm-browser-code-splitting.patch](patches/elm-browser-code-splitting.patch)).
Both are additive: a program with no async imports behaves identically.

An older elm/core or elm/browser compiles fine and then throws at the first
reference into a chunk, because nothing is listening for the marker. If you
see an uncaught `Error: An async-imported module is not here yet`, check
these two versions first.


## In elm reactor

Works, with no hashed sibling files: each chunk is served from the root
program's own endpoint, as `/src/Main.elm.mjs?chunk=Pages.Report`. Because
the page has to load the program as a module, `elm reactor` serves it
through `Main.elm.mjs` rather than inlining the bundle.


## Choosing where to split

The compiler splits exactly where you say and warns about nothing, so a
chunk behind a reference on a hot path will stall the app for a network
round-trip the first time it is taken. Split at the boundaries a person
would recognise -- a route, a rarely-opened dialog, an editor -- not at
whatever happens to be big.


## Upgrading

The optimized-AST format gained a constructor, so artifacts written by an
earlier build of this compiler no longer parse. Delete `elm-stuff` in your
projects and the stale package artifacts once:

```
rm -rf elm-stuff
find ~/.elm/webbhuset-* -name artifacts.dat -delete
```

Without it the compiler reports a corrupt cache and rebuilds anyway, which
works but is noisy.
