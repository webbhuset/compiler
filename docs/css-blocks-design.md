# CSS Blocks — Design Proposal

**Status: compiler side implemented on the `css-blocks` branch (parse,
typing, JS codegen, sidecar/inline CSS output). Not yet done: the `Css`
companion package (kernel code for `classes`/`class`/`vars`), and CSS name
shortening in `--optimize` (names are module-qualified in all modes for
now).**

Elm already embeds one foreign language with compiler support: GLSL. A
`[glsl| ... |]` block is parsed at compile time, its `attribute` / `uniform` /
`varying` declarations are extracted, and the expression gets the type
`WebGL.Shader { attrs } { uniforms } { varyings }`. Ordinary unification then
guarantees that Elm code and shader code agree.

This proposal applies the same idea to CSS. Today HTML is written in Elm but
CSS lives outside the compiler entirely: `class "card-title"` is a string, and
nothing notices when the stylesheet changes. That is a maintenance problem.

```elm
sheet : Css.Stylesheet { card : Css.Class, cardTitle : Css.Class } {}
sheet =
    [css|
        .card {
            border: 1px solid #ccc;
            border-radius: 4px;
        }
        .card:hover { border-color: #999; }
        .cardTitle { font-weight: bold; }
    |]


view : Model -> Html Msg
view model =
    let
        c = Css.classes sheet
    in
    div [ Css.class c.card ]
        [ h2 [ Css.class c.cardTitle ] [ text model.title ] ]
```

Delete `.cardTitle` from the CSS and every `c.cardTitle` fails to compile with
"this record does not have a field named `cardTitle`", listing the use sites.
Since a `Css.Class` value can only be obtained from a stylesheet, the reverse
direction is airtight too: the view cannot reference a class that no
stylesheet defines. Plain `Html.Attributes.class : String -> ...` remains
available for interop with external CSS, so the checking is opt-in per
element.


## The type

A CSS block has type `Css.Stylesheet classes vars` with two record parameters:

- **`classes`** — one field of type `Css.Class` per class selector found
  anywhere in the block (including inside `@media`, `@supports`, `@container`,
  and nested rules).
- **`vars`** — one field per custom property that the block *consumes but does
  not define* (see next section). `{}` when there are none.

Both records are closed. Unlike `WebGL.Shader`, whose parameters are phantom
(you only ever *supply* matching records), a stylesheet is a value you project
things out of, via the `Css` companion package:

```elm
module Css exposing (Stylesheet, Class, Value, ...)

classes   : Stylesheet classes vars -> classes          -- kernel
class     : Class -> Html.Attribute msg
classList : List ( Class, Bool ) -> Html.Attribute msg
vars      : Stylesheet classes vars -> vars -> Html.Attribute msg  -- kernel
```

The package ships kernel code and is consumed as a git-dependency, like the
patched `elm/core` for task ports. The compiler hard-codes the package and
type names (as it hard-codes `elm-explorations/webgl` for `WebGL.Shader`).

The module is named `Css`. This collides with `rtfeldman/elm-css`, so a
project cannot install both — accepted deliberately: the two are competing
answers to the same problem, and mixing them in one project would be
confusing anyway.


## Class names

**Class names in a block must be valid Elm record field names**
(lowerCamelCase). `.card-title` is a compile error with a hint to write
`.cardTitle`. Rationale: the names written in the block are only *keys* — the
emitted names are rewritten anyway (below) — and a kebab→camel conversion
would mean the name you write is never the name you see anywhere else, plus a
collision rule for `.card-title` vs `.cardTitle`. Requiring field names keeps
one name per class throughout.

Id selectors, element selectors, attribute selectors, and pseudo-classes/
-elements are allowed and passed through unchecked; only class selectors
participate in the type.

### Scoping and minification

Because every use of a class flows through the record, the compiler owns both
sides of the name and can rewrite it:

- **Dev / `--debug`:** classes are emitted module-qualified, e.g.
  `Page-Checkout--card`, giving CSS-Modules-style local scoping. Two modules
  can both declare `.card` without collision, and devtools show where a class
  came from.
- **`--optimize`:** class names go through the same shortening table as record
  fields (the shader codegen already does this for attribute names via
  `generateField`), so classes minify to one or two characters, consistently
  between the `.css` output and the JS.

The compiled stylesheet value carries a translation object
`{ card: "Page-Checkout--card", ... }`, exactly like the shader's
`attributes` / `uniforms` translation objects, which is what
`Css.classes` returns field access into.


## Custom properties

Custom properties are the analog of GLSL **uniforms**: typed inputs that Elm
supplies at runtime. This replaces the untyped `style "--x" (String.fromFloat
p ++ "%")` pattern and keeps all actual styling in CSS while values come from
the model.

The rule:

- A custom property **assigned** somewhere in the block (`--x: ...;`) is
  **internal**. It does not appear in the type. Internal names are scoped and
  minified like class names.
- A custom property **used** (`var(--x)`) or **registered** (`@property --x`)
  but never assigned is an **input**. It becomes a field in the `vars` record
  and must be provided from Elm.

```elm
sheet : Css.Stylesheet { bar : Css.Class } { progress : Css.Percentage }
sheet =
    [css|
        @property --progress { syntax: "<percentage>"; inherits: false; }

        .bar {
            width: var(--progress);
            transition: width 0.2s;
            background: var(--accent);
        }
    |]

view model =
    div
        [ Css.class c.bar
        , Css.vars sheet
            { progress = Css.pct model.progress
            , accent = Css.value "var(--brand-color)"
            }
        ]
        []
```

`Css.vars` walks the record (using a compiled translation table, since field
names are mangled under `--optimize`) and sets each property on the element as
an individual style entry, so multiple `vars` attributes on one element
compose, and virtual-dom diffs them normally.

### Typing inputs via `@property`

By default an input has the catch-all type `Css.Value` (constructed with
`Css.value : String -> Value` and typed helpers that lift into it). CSS
already has a syntax for declaring a custom property's type — the
`@property` rule — so the compiler uses it, mirroring how GLSL types map to
Elm types:

| `@property` syntax  | Elm field type   | constructed with                    |
|---------------------|------------------|-------------------------------------|
| *(no `@property`)*  | `Css.Value`      | `Css.value`, or lifting any of the below |
| `"<length>"`        | `Css.Length`     | `Css.px`, `Css.rem`, `Css.em`, ...  |
| `"<percentage>"`    | `Css.Percentage` | `Css.pct`                           |
| `"<color>"`         | `Css.Color`      | `Css.rgb`, `Css.hex`, ...           |
| `"<number>"`        | `Float`          | literal                             |
| `"<integer>"`       | `Int`            | literal                             |
| `"<time>"`          | `Css.Duration`   | `Css.ms`, `Css.s`                   |
| `"<angle>"`         | `Css.Angle`      | `Css.deg`, `Css.turn`, ...          |

`@property` rules are emitted to the CSS output (with scoped names), so the
browser-side benefits — animatable custom properties, type-checked values —
come along for free.

An `@property` rule with `initial-value` does not make the input optional:
the `vars` record always requires every input. `initial-value` only affects
browser behavior (what the property computes to before the first render sets
it). One rule, no optional-field machinery, and every use site states its
values explicitly.

### Interop with external CSS

Input names are scoped like everything else, so a block's `var(--accent)`
never accidentally reads a host-page variable. Consuming an external global is
explicit and goes through Elm, as in the example above: supply
`Css.value "var(--brand-color)"` and the browser's own var() substitution
resolves it at runtime. No special syntax, and the dependency on the external
name is visible (and greppable) in Elm code.

Sharing design tokens *between* blocks works the Elm way: define them once as
Elm constants (`gap = Css.px 4`) and pass them via `vars` wherever needed.
A third "exports" record parameter (the analog of GLSL varyings) that would
let one sheet's declared tokens be consumed by another is left as a future
extension.

### What placement is not checked

The type system guarantees an input's name and value type, not *where* it is
set. Setting `--progress` on an element that is not the styled element or one
of its ancestors silently does nothing — the same class of gap as GLSL not
checking your buffer contents. Convention: set `vars` on the same element as
the class that uses them.


## Keyframes and animations

`@keyframes` names live in the same global browser namespace as everything
else, so they need scoping too. They also have a nasty failure mode of their
own: a misspelled name in `animation:` is a silent no-op in every browser.
Both problems are solved together.

**Declaring.** `@keyframes fadeIn { ... }` names must be valid record field
names (like classes) and must not be one of the reserved animation keywords
(`none`, `infinite`, `alternate`, `forwards`, `paused`, `ease`, the other
timing/direction/fill/play-state keywords, and the CSS-wide keywords) —
compile error otherwise. This costs nothing: CSS itself cannot reference such
names from the `animation` shorthand, so these names are already broken in
plain CSS; the compiler just says so out loud. Names are scoped and minified
exactly like class names.

**Referencing.** Only two properties reference keyframes: `animation-name`
and the `animation` shorthand (plus their vendor-prefixed forms). The
compiler rewrites at the token level, inside those declarations only: any
identifier token that matches a name declared in the block becomes the scoped
name. Because keyword-colliding names are banned at declaration, this is
unambiguous **without implementing the shorthand's parsing rules** — the
keyword ban is the same disambiguation the browser itself relies on, applied
at compile time instead of parse-order tricks.

**Checking.** Inside those declarations, an identifier that is neither an
animation keyword nor a declared keyframes name is a compile error:

```
The animation name `fadeIn` is not defined by any @keyframes in this block.

    Hint: I found `fadeIin` — is that a typo?
```

This turns the browser's silent no-op into a compile error, which is the same
maintenance guarantee classes get. A declaration containing `var(...)` is
exempt from the check (its value cannot be known statically); that is also
the escape hatch below.

**Dynamic and cross-sheet animations.** Each `@keyframes` also becomes a
field in the `classes` record, typed `Css.Animation` (sharing a field
namespace with classes; a collision is a compile error). Together with
`Css.animationName : Animation -> Value` this makes animations first-class
values that flow through the existing `vars` mechanism:

```elm
sheet =
    [css|
        @keyframes slideIn { from { translate: -100% 0; } }
        @keyframes fadeIn  { from { opacity: 0; } }

        .toast { animation: var(--enter) 0.3s ease-out; }
    |]

view model =
    div
        [ Css.class c.toast
        , Css.vars sheet
            { enter =
                Css.animationName
                    (if model.reducedMotion then c.fadeIn else c.slideIn)
            }
        ]
        []
```

The same mechanism handles reuse of another sheet's keyframes (pass its
`Animation` value) and externally defined global animations
(`Css.value "spin"`), consistent with how external custom properties are
consumed — cross-boundary references always go through Elm, visibly.

**Scope unit.** Names (classes, vars, keyframes) are resolved per block;
emitted names are module-qualified, so declaring the same name in two blocks
of one module is a compile error. Sharing across blocks — same module or not
— goes through the record values.


## CSS file output

The CSS text is **not** embedded in the JS bundle. Output depends on the
`--output` target (`data Output` in `terminal/src/Make.hs`):

| target                  | JS/ESM output      | CSS output                        |
|-------------------------|--------------------|-----------------------------------|
| `--output=bundle.js`    | `bundle.js`        | `bundle.js.css` (sidecar)         |
| `--output=bundle.mjs`   | `bundle.mjs`       | `bundle.mjs.css` (sidecar)        |
| `--output=index.html`   | inlined `<script>` | inlined `<style>` in `<head>`     |
| *(no flag)*             | `index.html`       | inlined `<style>` in `<head>`     |
| `--output=/dev/null`    | nothing            | nothing                           |
| `elm reactor`           | served page        | inlined `<style>` in `<head>`     |

The sidecar name is the JS name plus `.css`, following the source-map
convention (`bundle.mjs.map`), so it is unambiguous and bundler-friendly. The
user links it themselves: `<link rel="stylesheet" href="bundle.mjs.css">`.
A sidecar is written only when the compiled program contains at least one
live CSS block.

Contents and ordering:

- Only blocks **reachable after dead-code elimination** are included. Each
  block is one node in the `Opt` graph, so a sheet used from many places is
  emitted exactly once, and sheets in dead code are dropped — same machinery
  as everything else.
- Blocks are emitted in the same deterministic order as JS definitions
  (topological module order, source order within a module), so the cascade is
  stable across builds. Scoped class names make cross-block cascade order
  mostly irrelevant anyway.
- Dev builds prefix each block with `/* Module.Name (src/Module/Name.elm) */`;
  `--optimize` minifies whitespace and uses the shortened names.


## What is checked, what is not

Checked at compile time:

- Every class the view uses exists in some stylesheet (by construction).
- Every class/var name change in CSS surfaces as type errors at the use sites.
- Every input custom property is supplied, with a value of the declared type.
- Every animation name in `animation`/`animation-name` resolves to a
  `@keyframes` in the block (misspelled animation names are silent no-ops in
  browsers).
- Structural validity: the block is tokenized per the CSS Syntax Module and
  selectors are parsed properly, so unbalanced braces and malformed selectors
  are compile errors with positions.

Not checked (deliberately out of scope):

- Property-level validity (`colr: bleu` passes; the GLSL feature fully
  parses its language, this proposal does not).
- That a class is applied to a sensible element, or class co-occurrence
  (`.btn--primary` only with `.btn`).
- Custom-property placement/inheritance (above).
- Unused classes are not an error (unlike shader uniforms, which must all be
  supplied). They are *detectable* — a class never projected from the record
  is provably dead — and could become a `Nitpick` diagnostic later.


## Implementation map

The feature follows the shader rails end to end; nothing touches the unifier
or the solver.

| step | GLSL today | CSS blocks |
|------|------------|------------|
| trigger | `[glsl\|` in `Parse/Expression.hs` | add `[css\|` beside it; the `[... \|]` block scanner in `Parse/Shader.hs` is reusable |
| parse | `language-glsl` package | hand-written CSS tokenizer (CSS Syntax Module, ~200 lines) + selector parser; records byte offsets of class/var tokens for rewriting |
| AST | `Src.Shader src types` | `Src.CssBlock src interface` through `Canonical` and `Optimized`; new `Opt` node tag → elm-stuff/interface cache invalidation, as with task ports |
| typing | `constrainShader` (`Type/Constrain/Expression.hs:468`) builds `WebGL.Shader` record types | same shape: build `Css.Stylesheet (RecordN classes) (RecordN vars)`; package/type names hard-coded in `Elm.ModuleName` like `ModuleName.webgl` |
| codegen | JS object `{src, attributes, uniforms}` with `generateField` renaming in `--optimize` | JS object `{classes: {...}, vars: {...}}` translation tables (no CSS text); reuse `generateField` for shortening |
| output | — | collect live `Opt.CssBlock` sources during generation; write sidecar / inline in `terminal/src/Make.hs` next to the existing `JS`/`Esm`/`Html` cases |
| runtime | `elm-explorations/webgl` kernel | small `Css` package with kernel code, via git-dependencies |

Estimated scope is comparable to the shader feature and smaller than
comparable-newtypes. The riskiest piece is the CSS tokenizer, which is boring
risk, not architectural risk.


## Future extensions

- **Cross-sheet token exports** (a third record parameter, the analog of GLSL
  varyings) that would let one block's declared tokens be consumed by another
  block's CSS directly. Explicitly not v1: sharing goes through Elm values
  (`Class`, `Animation`, `vars`), which may prove sufficient permanently.
- **Dead-class detection** as a `Nitpick` diagnostic (a class never projected
  from the record is provably unused).


## Alternative considered

A build-step generator outside the compiler (parse `.css` files, emit
`Styles.elm` with `card : Class` values — what typed CSS Modules do in
TypeScript) provides the name-safety with no compiler changes. It cannot
provide colocation, inferred record types without a generate step,
typed custom-property inputs, or optimize-integrated class mangling. The
scoping/minification story is what justifies the compiler route, so v1
includes it rather than treating it as a follow-up.
