# ES Module Output

This fork can compile to an **ES module** instead of the classic
IIFE-that-assigns-`window.Elm` bundle. Name the output `.mjs` to enable it:

```
elm make src/Main.elm --output=elm.mjs
elm make src/Main.elm src/Pages/Home.elm --optimize --output=assets/elm.mjs
```

The module has one named export, `Elm`, which is also the default export.
It has the same shape as the classic `window.Elm`, including nesting for
dotted module names and multiple mains compiled into one file:

```js
import { Elm } from "./elm.mjs";
// or: import Elm from "./elm.mjs";

const app = Elm.Main.init({ node: document.getElementById("root") });
const home = Elm.Pages.Home.init({ ... });
```

This works in browsers (`<script type="module">`), in Node.js, and with
bundlers (Vite, esbuild, webpack, ...) without any wrapper glue. Task ports
and regular ports behave exactly the same as in `.js` output.

Differences from `.js` output:

- Nothing is assigned to the global scope; the only way in is the import.
- Several `.mjs` files on one page do **not** merge into a shared `Elm`
  object the way multiple classic bundles merge into `window.Elm` — each
  module stands alone.
- `--output=foo.js` and `--output=foo.html` are unchanged, byte-for-byte.

The compiler always produces a single module, even with multiple inputs;
shared-code splitting is left to bundlers, which handle a single ESM entry
point well.
