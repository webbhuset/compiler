# Web Workers — Design Notes

**Status: implemented. Compiler side on the `css-blocks` branch; the
`webbhuset/worker` companion package (kernel + effect manager) lives in a
separate repository. Requires `--output=something.mjs`.**

Elm has no way to use web workers without leaving the language: you compile
a second program with a second `elm make`, wire `postMessage` by hand, and
write JSON encoders/decoders for every message — which drift. This feature
makes workers native: a worker is an Elm module compiled *in the same
invocation* as its spawner, so messages cross the boundary as ordinary Elm
values.

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

`elm make src/Main.elm --output=main.mjs` writes `main.mjs` plus one
`main.<hash>.mjs` per spawned worker program.


## Why no JSON is needed

`postMessage` uses structured clone, and Elm's runtime representation —
plain objects, arrays, strings, numbers — clones fine. What normally makes
this impossible is `--optimize`: two separately compiled bundles disagree
about constructor tags and record field names. Compiled in one invocation,
both bundles share one rename table (the same argument that made CSS name
mangling work), so representations agree by construction: custom types
cross the boundary as-is.

The remaining constraint is structured clone itself: **functions do not
clone**. `args`, `toParent`, and `msg` types must be function-free. In this
version that is a runtime error (a `DataCloneError` surfaces as `onCrash`
from the worker side, or a console error from `send`); a static check on
the worker `main`'s annotation — the same shape as the comparable-newtypes
machinery — is the natural future refinement, and is the reason worker
programs must be annotated top-level `main` values in the first place.


## The types

```elm
type Program args toParent msg model  -- what a worker module's main is
type Worker msg                       -- parent's handle: send + kill
type Channel msg                      -- send-end of one typed link

worker        : { init : args -> Channel toParent -> ( model, Cmd msg ), update, subscriptions } -> Program ...
spawn         : Program args toParent msg model -> args -> { onSpawn : Worker msg -> parentMsg, onMessage : toParent -> parentMsg, onCrash : String -> parentMsg } -> Cmd parentMsg
channel       : Worker msg -> Channel msg
send          : Channel msg -> msg -> Cmd a
kill          : Worker msg -> Cmd a
stop          : Cmd a                 -- worker terminates itself
```

Decisions worth recording:

- **Communication is strictly parent↔child links created at spawn**, so
  the handle is a channel (the send-end of one typed link), not a PID —
  nothing is addressable, there is no registry. The worker's `init`
  receives its parent's `Channel toParent`; the parent gets a
  `Worker msg` from `onSpawn`.
- **The capability split is in the types**: the parent's `Worker msg` can
  send and `kill`; the child's `Channel` can only send. A worker cannot
  kill its parent by construction. `stop` (self-termination) is ambient —
  it needs no handle.
- **No exit payload type.** An earlier sketch had `PID msg exit` with an
  `exit` handler. Anything a worker wants to say when leaving is an
  ordinary `toParent` message sent before `stop` (ops in one command batch
  run in order, so the send is flushed first). This keeps `Program` at four
  type parameters and termination notification out of the send-handle.
- **`spawn` is a `Cmd`, not a `Task`.** The original sketch returned
  `Task Never (PID msg exit)`, but a task resolves exactly once, while a
  spawned worker delivers messages forever. The only sanctioned way for
  kernel code to inject messages into an app at arbitrary later times is an
  effect manager's router (`Platform.sendToApp`) — the same machinery ports
  use — and routers exist only inside effect managers, which speak in
  commands. Hence `spawn` with `onSpawn`/`onMessage`/`onCrash` mappers.
- **Channels are backed by the worker's message port.** They are not
  transmittable (they close over the port), so worker↔worker topology
  routes through the parent. `MessagePort` transfer would enable direct
  links, but transfer *neuters* the sender's copy — a value-semantics
  violation that needs explicit design (some `Worker.transfer` operation,
  never a channel riding silently in a message). Future work.


## Compile pipeline

Follows the CSS-blocks rails, with multi-bundle output as the new part:

1. **Validation** (`Nitpick.Workers`, run from `Compile.compile`): every
   use of `Worker.spawn` must be a direct three-argument call whose first
   argument is a direct reference to a top-level value. Anything else —
   partial application, aliasing, a computed program — is a compile error,
   since the compiler must know statically which values become bundles.
2. **Worker `main`s** are ordinary definitions: `Optimize.Module` accepts
   `Worker.Program _ _ _ _` as a valid `main` type without treating the
   module as a program root.
3. **Rewrite** (`Optimize.Expression.optimizeArgs`): at a spawn call, the
   program argument compiles to `Opt.WorkerRef global` (binary tag 28) and
   deliberately registers *no* dependency on the referenced global — the
   worker's code must not land in the spawning bundle. Everything else
   about the call is vanilla; `spawn` itself is an ordinary package
   function.
4. **Planning** (`Generate.Workers.plan`): walk the live graph from the
   mains collecting `WorkerRef`s, then depth-first through each worker's
   own live graph (workers can spawn workers). The result is in dependency
   order; spawn cycles are a compile error since content-hashing cannot
   order them.
5. **Codegen**: `Opt.WorkerRef` emits a NUL-delimited placeholder token as
   a JS string. Each worker bundle (`Generate.JavaScript.generateWorkerBundle`)
   is the kernel prelude + the graph reachable from the program global +
   `_Worker_run(<program>);`, generated with the same `Mode` as the main
   bundle — that shared rename table is what makes structured-clone
   representations agree.
6. **Finalize** (`builder Generate.finalize`): render worker bundles in
   dependency order; substitute the (already final) file names of the
   workers each bundle spawns; SHA-1 the bytes; name the file
   `<base>.<hash16>.mjs`; finally substitute all names into the main
   bundle. Both bundles begin with
   `var __elmWorkerBaseUrl = import.meta.url;` and the kernel resolves
   worker URLs against it — which is why workers require ESM output
   (`import.meta` is illegal syntax elsewhere); `.js`, `.html`, and
   `elm reactor` report an error when the program spawns workers.
7. **CSS blocks compose**: stylesheet collection roots at worker programs
   too, so `[css| ... |]` used inside a worker lands in the single sidecar.


## Runtime (webbhuset/worker)

An `effect module` with kernel code, consumed as a git-dependency (kernel
and effect managers are trusted there):

- Commands compile to opaque ops; `onEffects` executes them with the
  router. `spawn` creates the `Worker`, wires `onmessage`/`onerror` to
  `Platform.sendToApp router ∘ mapper`, posts the INIT message, and
  delivers the handle via `onSpawn`.
- The worker-side entry `_Worker_run` is `_Platform_initialize` re-wired:
  same managers/`sendToApp`/effects-queue calls from the core kernel, but
  the program initializes from the INIT message (args + parent channel
  instead of flags) and subsequent messages feed `update` directly. Full
  effect-manager machinery works inside workers — subscriptions, tasks,
  HTTP — anything that does not need the DOM.
- Message protocol: parent→worker `{$:0, a: args}` then `{$:1, a: msg}`;
  worker→parent messages travel raw.
- The browser queues messages posted before a worker's module finishes
  loading, so `send` right after `spawn` has no race, and no kernel-side
  queue is needed.


## What is checked, what is not

Checked at compile time: spawn shape (direct call, top-level program),
worker `main` type, ESM output requirement, spawn cycles.

Not checked (runtime behavior): function-free message types (structured
clone throws; see above), and message volume/size (structured clone copies
— a huge model sent 60×/s is a real cost; workers are for coarse-grained
messaging).


## Future extensions

- **Static transmittable check** on the worker `main` annotation
  (`Type.Comparable`-style interface closure), with `Channel`/`Worker`
  explicitly excluded.
- **Transferable channels** for parent-brokered worker↔worker links (with
  the neutering problem solved explicitly).
- **Standalone worker builds**: `elm make src/Counter.elm --output=counter.mjs`
  for use from hand-written JS.
- **Stale bundle cleanup**: hashes change with content, so old
  `<base>.<hash>.mjs` files accumulate in the output directory; the
  compiler could prune siblings matching the pattern.
