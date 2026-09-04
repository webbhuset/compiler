# Compiled pieces in elm reactor

`elm reactor` turns a `.elm` file into a page: `/src/Main.elm` compiles the
module and wraps the result in HTML. That page is the reactor's, not yours. You
cannot give it a `<meta name="viewport">`, wire up ports, or load the program as
a module, and a program that spawns workers cannot be shown at all.

So the reactor also serves the compiled program in pieces, at the names
`elm make` would have written:

| request              | serves                                   | compiled as            |
|----------------------|------------------------------------------|------------------------|
| `/src/Main.elm.js`   | the program as a classic bundle          | `--output=main.js`     |
| `/src/Main.elm.css`  | its stylesheet, from CSS blocks          | (empty if it has none) |
| `/src/Main.elm.mjs`  | the program as an ES module              | `--output=main.mjs`    |

Each request compiles the module then and there, so a reload is a rebuild, and
nothing is written to disk.


## A page of your own

Put an HTML file next to your sources and open it through the reactor, at
`http://localhost:8000/src/dev.html`:

```html
<!doctype html>
<meta name="viewport" content="width=device-width, initial-scale=1">
<link rel="stylesheet" href="/src/Main.elm.css">

<div id="app"></div>

<script src="/src/Main.elm.js"></script>
<script>
  var app = Elm.Main.init({ node: document.getElementById("app"), flags: 42 });
  app.ports.toJs.subscribe(function (msg) { console.log(msg); });
</script>
```

Ports, flags, the viewport, whatever other scripts you need: all yours.


## Workers

A program that spawns workers has to be loaded as a module, since a worker is
found relative to the bundle that spawns it and only a module knows its own
URL:

```html
<script type="module" src="/src/Main.elm.mjs"></script>
```

Instead of the hashed sibling files `elm make` writes, the reactor points each
spawn at the worker module's own URL. A `Worker.spawn Counter.main` in
`Main.elm` loads `/src/WebWorker/Counter.elm.mjs`, and requesting that compiles
`Counter.elm` as a worker program. There is nothing to keep in sync and nothing
to clean up. A worker's URL is stable across edits, and the reactor answers
with `Cache-Control: no-store` so the browser does not keep a stale one.

This works one level down as well, for a worker that spawns workers, and for a
worker in any of the project's source directories. A worker that lives in a
*package* has no source file under the project to serve, and asking for a
bundle that spawns one is an error saying so.


## When the build fails

A `.js` or `.mjs` request that fails to compile answers with status 500 and a
script that logs the compiler's report with `console.error`, so it turns up in
the devtools console rather than as a silent broken page. A failed `.css`
request is a 500 with an empty body.
