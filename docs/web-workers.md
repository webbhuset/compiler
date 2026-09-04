# Web Workers

This fork supports **native web workers**: a worker is an Elm module
compiled into its own JavaScript file by the same `elm make` that compiles
the app spawning it. Messages cross the boundary as ordinary Elm values —
custom types included, no JSON encoders — because both sides are compiled
together and share one representation, even under `--optimize`.

## Usage

A worker is a module whose `main` is a `Worker.Program`:

```elm
module Counter exposing (Args, Msg(..), ToParent(..), main)

import Browser.Worker as Worker


type alias Args =
    { initial : Int }

type ToParent
    = CurrentValue Int

type Msg
    = Inc Int


main : Worker.Program Args ToParent Msg Model
main =
    Worker.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Args -> Worker.Channel ToParent -> ( Model, Cmd Msg )
init args parent =
    ( { value = args.initial, parent = parent }
    , Worker.send parent (CurrentValue args.initial)
    )
```

`init` receives the spawner's channel for sending `toParent` messages
upward. Everything else is a normal Elm program: `update`, `subscriptions`,
commands, tasks, HTTP — anything that does not need the DOM.

Spawn it by referencing its `main` directly:

```elm
import Browser.Worker as Worker
import Counter


type Msg
    = GotCounter (Worker.Worker Counter.Msg)
    | FromCounter Counter.ToParent
    | CounterCrashed String


init : () -> ( Model, Cmd Msg )
init () =
    ( ...
    , Worker.spawn Counter.main
        { initial = 10 }
        { onSpawn = GotCounter
        , onMessage = FromCounter
        , onCrash = CounterCrashed
        }
    )
```

`onSpawn` delivers a `Worker.Worker Counter.Msg` handle; send to it with
`Worker.send (Worker.channel handle) (Counter.Inc 5)`. Messages sent right
after `spawn` are queued by the browser until the worker is ready.

## Output files

Workers require ES module output:

```
elm make src/Main.elm --output=main.mjs
```

This writes `main.mjs` plus one `main.<hash>.mjs` per spawned worker
program, named by content hash. The files must stay siblings — worker URLs
resolve relative to the bundle (`import.meta.url`), so copy them together.
Old hashes are not deleted; clean stale `main.*.mjs` files when deploying.

A worker program can also be compiled on its own. `elm make src/Counter.elm
--output=counter.mjs` writes just that worker's bundle, which is how the
reactor serves one.

The classic `.js` and `.html` outputs cannot host workers (no `import.meta`),
and compiling a worker-spawning program to them is an error. In `elm reactor`,
load the program as a module from a page of your own — see
[reactor.md](reactor.md), which serves each worker from its own source path
rather than as a hashed sibling. Workers can spawn workers; a spawn *cycle* is
a compile error.

## Messages and the boundary

- `args`, `toParent`, and the worker's `msg` cross the boundary as
  structured clones. Custom types, records, lists, dicts — all fine.
  **Functions do not clone**: a message containing a function fails at
  runtime (`onCrash` from the worker side, a console error from `send`).
- Values are *copied*, not shared. Coarse-grained messages are cheap;
  shipping a huge model on every animation frame is not what workers are
  for.
- `Worker.Worker` and `Worker.Channel` cannot be sent in messages, so
  worker↔worker communication routes through the parent.

## Lifecycle

- `Worker.stop : Cmd a` — inside a worker: terminate after the current
  update. Sends batched in the same update are flushed first, so a final
  `toParent` message before `stop` arrives.
- `Worker.kill : Worker msg -> Cmd a` — parent-initiated termination.
  Messages the worker already sent still arrive.
- `onCrash` fires when the worker throws, fails to load, or a message from
  it cannot be cloned.
- A worker's channel can only send: a worker cannot kill its parent.

## Rules

- `Worker.spawn` must be called directly with all three arguments, and the
  program must be a direct reference to a top-level value
  (`Worker.spawn Counter.main args handlers`). Partial application,
  aliasing, or computing the program is a compile error — the compiler
  needs to see, at compile time, which value becomes a bundle.
- A worker module's `main` must be annotated with its `Worker.Program`
  type. It is not an app `main`: `elm make src/Counter.elm` alone is not a
  valid program root.

## Runtime: patched elm/browser

`Browser.Worker` lives in a fork of `elm/browser` (kernel code plus an
effect manager), consumed as a [git dependency](git-dependencies.md) under
an unpublished version number, like the elm/core patches for task ports:

```json
"dependencies": {
    "direct": { "elm/browser": "1.100.201", ... }
},
"git-dependencies": {
    "elm/browser": "git@github.com:webbhuset/elm-browser.git"
}
```

The addition is in
[patches/elm-browser-worker.patch](patches/elm-browser-worker.patch); it is
purely additive, so programs not using workers behave identically. No
elm/core or elm/virtual-dom patches are needed.

Design rationale — why `spawn` is a `Cmd` rather than a `Task`, the
`Worker`/`Channel` capability split, and the compile pipeline — is in
[web-workers-design.md](web-workers-design.md).
