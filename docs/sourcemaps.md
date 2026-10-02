# Source maps

```
elm make src/Main.elm --sourcemap --output=main.mjs
```

writes `main.mjs.map` next to `main.mjs`, and a map next to every chunk and
worker file the build writes, each file ending with a
`//# sourceMappingURL=` comment that points at its map. Profilers, stack
traces and debuggers then name the Elm definition the running code came
from, instead of a line in a large generated file.

It works with `--output=something.js` and `something.mjs`, with or without
`--optimize`. HTML output has the script inline, so there is no file for a
map to describe, and `--sourcemap` is refused there.


## What is mapped

Every line of the JavaScript a top level definition compiles to points at
the line that definition starts on: the `view model =` line, not the line
inside `view` that is running. That is enough for a profiler to say where
time goes, and for a stack trace to say which function failed:

```
Error: TODO in module `Heavy` on line 7
    at _Debug_crash (elm-packages/elm/core/1.100.505/Elm/Kernel/Debug.js:273:1)
    at $author$project$Heavy$greeting (src/Heavy.elm:5:1)
    at init (src/Main.elm:10:1)
```

Package code is mapped the same way, and kernel JavaScript is mapped line
by line to its `.js` file, since it is copied over almost as written.
Constructors map to their line in the custom type, and a group of mutually
recursive definitions to its first member. The runtime glue around the
definitions is left unmapped.

The Elm sources are embedded in the map (`sourcesContent`), so nothing has
to be served for DevTools to show them. Project files are named by their
path relative to the output directory, like `../src/Main.elm`; package
files as `elm-packages/<author>/<package>/<version>/...`.


## Bundlers

esbuild, Rollup and webpack read an input file's `sourceMappingURL` and
compose its map into theirs, so the Elm lines survive bundling and
minifying:

```
elm make src/Main.elm --sourcemap --output=elm/main.mjs
esbuild app.mjs --bundle --minify --sourcemap --outfile=dist/app.js
```

The minified names come back too, because the map points at the original
JavaScript definitions.


## The output is unchanged

The JavaScript is byte for byte what the same build without `--sourcemap`
writes, apart from the comment at the end, and chunk and worker files keep
their content-hashed names. The compiler does not track output positions:
it puts a marker in front of each definition's code, notes the line each
marker lands on once a file is rendered, and takes the markers out again
before anything is hashed or written. See `compiler/src/Generate/SourceMap.hs`.
