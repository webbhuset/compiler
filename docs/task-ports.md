# Task Ports

This fork supports **task ports**: ports whose type is a `Task`, backed by a
promise-returning function on the JavaScript side. They make request/response
style FFI (HTTP calls, database queries, file access on Node.js, ...) a single
`Task` instead of an outgoing/incoming port pair.

## Usage

Declare a port producing a `Task` in a `port module`:

```elm
port fetchUser : { id : String } -> Task Json.Decode.Value { name : String }
```

Use it like any other task — it composes with `Task.andThen`, `Task.map`,
`Task.sequence`, and runs via `Task.attempt` / `Task.perform`:

```elm
fetchUser { id = "42" }
    |> Task.attempt GotUser
```

Provide the implementation when starting the program. Anything
`Promise.resolve` accepts works: an `async` function, a function returning a
promise, or a plain synchronous function.

```js
var app = Elm.Main.init({
    node: ...,
    flags: ...,
    taskPorts: {
        fetchUser: async (args) => {
            const res = await fetch("/api/users/" + args.id);
            return res.json();
        }
    }
});
```

## Rules

- The type must be `args -> Task Json.Decode.Value result`, with exactly one
  argument. Use `()` if the JavaScript side needs no input.
- The argument and result types follow the same rules as normal port
  payloads (JSON-friendly types); encoders and decoders are derived by the
  compiler exactly like for `Cmd`/`Sub` ports.
- The error type must be `Json.Decode.Value`, because JavaScript can reject
  a promise with any value at all. Decode it on the Elm side if you need
  something more precise. Note that `Error` objects pass through as values,
  so `Json.Decode.field "message" Json.Decode.string` recovers the message
  of a rejected `new Error(...)`.

The task fails with a descriptive `Error` value when no implementation was
registered for the port, when the implementation throws or rejects, and when
the resolved value does not match the declared result type (in non-optimized
builds the message includes the full JSON error).

Because implementations are passed to `init`, they are always registered
before the program's `init` commands run — a task port fired from `init`
just works. The registration table is shared per compiled bundle: if you
initialize several programs from one bundle, the task port implementations
passed to the most recent `init` win for names they share.

Cancellation is not supported: killing the surrounding process (e.g. via
`Task.attempt` on a dead app) simply drops the eventual result; the
JavaScript promise itself is not aborted.

## Runtime: patched elm/core

The compiler generates calls to `_Platform_taskPort`, which lives in
`elm/core`'s kernel code, so task ports need a **patched elm/core**. Apply
[patches/elm-core-task-ports.patch](patches/elm-core-task-ports.patch) to a
fork of elm/core, tag it with a [fork
version](git-dependencies.md#numbering-a-fork-of-a-published-package), and
point your project at the fork with a [git
dependency](git-dependencies.md):

```json
"dependencies": {
    "direct": { "elm/core": "1.100.503", ... }
},
"git-dependencies": {
    "elm/core": "git@github.com:webbhuset/core.git"
}
```

The patch is additive: the patched elm/core behaves identically for programs
that use no task ports.
