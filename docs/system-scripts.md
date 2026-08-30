# Command line scripts

This fork can compile Elm to programs that run on the command line. A
script is a module whose `main` takes the process it is running as and
produces a task:

```elm
module Hello exposing (main)

import System
import Task exposing (Task)


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

The output runs itself — it gets a `#!/usr/bin/env node` line and the
executable bit — so there is no wrapper to write and nothing to call
`init` on.

## The type is the contract

`main : System.Process -> Task String Int` says everything about how the
program ends:

- succeeding with an `Int` exits with that status code,
- failing with a `String` prints it to standard error and exits with 1.

So the last thing most scripts do is turn their errors into a message:

```elm
main : System.Process -> Task String Int
main process =
    run process
        |> Task.mapError System.Error.format
```

## The process

```elm
type alias Process =
    { argv : List String
    , env : Dict String String
    , platform : String
    }
```

`argv` holds *your* arguments: the node binary and the script path are
already removed, so `./tool a b` gives `["a","b"]`. Everything here is a
snapshot from startup; things that change while the program runs (the
working directory) are tasks instead.

## Writing a script

`Task.await` (in this fork's elm/core) is `Task.andThen` with the task
first, which is what the [back-lambda](back-lambdas.md) syntax wants:

```elm
import System
import System.Error
import System.File
import Task exposing (Task)


main : System.Process -> Task String Int
main process =
    (\source <- Task.await (System.File.read "input.txt")
     \_ <- Task.await (System.File.write "output.txt" (String.toUpper source))

     System.stdout "done\n"
        |> Task.map (\_ -> 0)
    )
        |> Task.mapError System.Error.format
```

The same code written with `andThen` and nested lambdas does exactly the
same thing; pick whichever reads better.

## What is available

**`System`** — `stdout`, `stderr`, `stdin` (reads until end of input),
`isTerminal` (false when output is piped, which is when to skip colors),
`cwd`, `chdir`, and `exit` for stopping early.

**`System.File`** — `read`, `write`, `append`, `exists`, `isDirectory`,
`size`, `remove`, `rename`, `copy`, `makeDirectory` (creates parents),
`removeDirectory`, `list`.

**`System.Error`** — the failure vocabulary, and `format` to turn any of
it into a line worth printing.

## Errors

Errors are [structural variant tags](structural-variants.md), so each
operation says what it can actually fail with — `read` cannot claim
`DirectoryNotEmpty` — and chaining unions the rows on its own:

```elm
read : String -> Task [ r | NotFound String, PermissionDenied String, IsADirectory String, Unknown Details ] String
```

Because the rows stay open, you can handle one tag and pass the rest
along. The catch-all is typed at the row *minus* what was matched, so the
caller only sees what is genuinely left:

```elm
readOrEmpty : String -> Task [ r | PermissionDenied String, IsADirectory String, Unknown Details ] String
readOrEmpty path =
    System.File.read path
        |> Task.onError
            (\err ->
                case err of
                    NotFound _ ->
                        Task.succeed ""

                    other ->
                        Task.fail other
            )
```

An errno this package does not have a tag for becomes `Unknown`, carrying
the system's own code and message, so nothing is lost.

Note that `Task.attempt` produces a `Result`, and a tag pattern cannot
currently sit inside a constructor pattern, so `Err NotFound ->` does not
compile. Match the `Result` first and the tag in a second `case`, or stay
with `onError`/`mapError`, which are unaffected.

## Rules

- A script must be the only program compiled: the bundle runs itself, so
  it cannot also export programs for a page to start.
- `--output` must be `.js` or `.mjs`. Web page targets and `elm reactor`
  report an error.
- The DEV mode console warning is not emitted for scripts, since a
  program's standard error is part of its contract.

## Runtime: the webbhuset/system package

`System` and its submodules live in the `webbhuset/system` package, which
contains kernel code and is therefore consumed as a
[git dependency](git-dependencies.md):

```json
"dependencies": {
    "direct": { "webbhuset/system": "1.0.0", ... }
},
"git-dependencies": {
    "webbhuset/system": "git@gitlab.webbhuset.com:webbhuset/internal/frontend/elm-system.git"
}
```

Long running programs that need to react to events — a watcher, a server,
signal handling — want a message loop rather than a single task. Write
those as a `Platform.worker` with ports; the `System.*` tasks work there
unchanged.
