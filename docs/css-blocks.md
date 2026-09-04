# CSS Blocks

This fork supports **CSS blocks**: CSS embedded in Elm the way GLSL shaders
are, with a type inferred from the CSS itself. Classes, `@keyframes`, and
custom properties become record fields, so the compiler catches HTML and CSS
drifting apart — remove a class from the CSS and every use site becomes an
ordinary type error.

## Usage

Write CSS in a `[css| ... |]` block and use its classes through the record
returned by `Css.classes`:

```elm
import Css


sheet =
    [css|
        .card {
            border: 1px solid #ccc;
            border-radius: 4px;
        }

        .card:hover { border-color: #999; }

        .cardTitle { font-weight: bold; }
    |]


view model =
    let
        c = Css.classes sheet
    in
    div [ Css.class c.card ]
        [ h2 [ Css.class c.cardTitle ] [ text model.title ] ]
```

The block's type is inferred:

```elm
sheet : Css.Stylesheet { card : Css.Class, cardTitle : Css.Class } {}
```

A `Css.Class` can only be obtained from a stylesheet, so an element can
never reference a class that no stylesheet defines. Use
`Css.classList [ ( c.selected, model.selected ) ]` for conditional classes.
Plain `Html.Attributes.class "..."` still works for CSS that lives outside
Elm.

## Custom properties

A custom property that a block *uses* but never *assigns* is an **input**:
it appears in the second record of the type and must be supplied from Elm
with `Css.vars`, per element. An `@property` rule types the input through
its `syntax` descriptor:

```elm
sheet :
    Css.Stylesheet
        { bar : Css.Class }
        { progress : Css.Percentage, accent : Css.Value }
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
        (Css.class c.bar
            :: Css.vars sheet
                { progress = Css.pct model.progress
                , accent = Css.value "var(--brand-color)"
                }
        )
        []
```

- `"<percentage>"` gives `Css.Percentage`, `"<length>"` `Css.Length`,
  `"<color>"` `Css.Color`, `"<time>"` `Css.Duration`, `"<angle>"`
  `Css.Angle`, `"<number>"` `Float`, `"<integer>"` `Int`. Inputs without an
  `@property` rule are `Css.Value`, built with `Css.value` or lifted from
  the typed constructors (`Css.px 4 |> Css.length`, ...).
- A property the block assigns somewhere (`--x: ...;`) is internal: scoped,
  not in the type.
- To read a global defined outside Elm (a design token on `:root`, say),
  pass it explicitly: `Css.value "var(--brand-color)"`. Block-local names
  never collide with the page's globals.
- Set `Css.vars` on the element that uses the properties (or an ancestor —
  they inherit). The type system checks names and value types, not
  placement.

## Keyframes and animations

`@keyframes` names become `Css.Animation` fields in the classes record. In
`animation` and `animation-name` values, any identifier that is neither an
animation keyword nor a declared `@keyframes` is a compile error — a
misspelled animation name is a silent no-op in browsers, but not here.
Select animations dynamically through an input:

```elm
sheet =
    [css|
        @keyframes slideIn { from { translate: -100% 0; } }
        @keyframes fadeIn  { from { opacity: 0; } }

        .toast { animation: var(--enter) 0.3s ease-out; }
    |]


view model =
    div
        (Css.class c.toast
            :: Css.vars sheet
                { enter =
                    Css.animationName
                        (if model.reducedMotion then c.fadeIn else c.slideIn)
                }
        )
        []
```

## Where the CSS goes

The CSS text is never embedded in the JavaScript bundle:

| target                | CSS output                          |
|-----------------------|-------------------------------------|
| `--output=bundle.js`  | `bundle.js.css` sidecar             |
| `--output=bundle.mjs` | `bundle.mjs.css` sidecar            |
| `--output=index.html` | inlined `<style>` in the page       |
| `elm reactor`         | inlined in the `.elm` page, or a `Main.elm.css` sidecar for [your own page](reactor.md) |

Link the sidecar yourself:
`<link rel="stylesheet" href="bundle.mjs.css">`. It contains exactly the
blocks that survive dead-code elimination — stylesheets in unused code cost
nothing — and is only written when the program has CSS blocks.

Emitted names are module-scoped: class `card` in module `Page.Checkout` is
emitted as `Page-Checkout--card`, and custom properties and keyframes
likewise. Two modules can both declare `.card` without collision, and
devtools show where a class came from. With `--optimize`, names are instead
shortened to one or two characters — still collision-free across modules —
and the per-module comments are dropped. The JavaScript bundle holds only
small translation tables in either mode.

## Rules

- Class names, custom property names (after the `--`), and `@keyframes`
  names must be lowerCamelCase, since they become record fields. The error
  for `.card-title` suggests `.cardTitle`.
- Declaring the same name twice in one module, a class/keyframes name
  collision, or a `@keyframes` named after an animation keyword
  (`infinite`, `ease`, ...) is a compile error.
- The CSS is tokenized and structurally validated (balanced braces, class
  selectors, `var()` uses), with errors pointing into the block. Property
  *values* are passed through as written — `colr: bleu` is not caught.
- Everything CSS can do is available: media queries, container queries,
  nesting, pseudo-elements. Only class selectors, custom properties, and
  keyframes participate in the type.

## Runtime: the webbhuset/css package

`Css.Stylesheet` and friends live in the `webbhuset/css` package, which
contains kernel code and is therefore consumed as a
[git dependency](git-dependencies.md):

```json
"dependencies": {
    "direct": { "webbhuset/css": "1.0.0", ... }
},
"git-dependencies": {
    "webbhuset/css": "git@gitlab.webbhuset.com:webbhuset/internal/frontend/elm-css.git"
}
```

`Css.vars` sets custom properties as element styles, which the stock
`elm/virtual-dom` cannot do (it assigns `element.style[key] = value`;
browsers require `style.setProperty` for `--custom` properties). Apply
[patches/elm-virtual-dom-custom-properties.patch](patches/elm-virtual-dom-custom-properties.patch)
to a fork of elm/virtual-dom, tag it with a [fork
version](git-dependencies.md#numbering-a-fork-of-a-published-package)
(`1.100.502` for the fork this compiler expects), and consume it as a git
dependency, like the elm/core patches for task ports. Everything except
`Css.vars` works with the stock virtual-dom. Note that `setProperty` only
accepts hyphenated names, so camelCase keys in `Html.Attributes.style`
(`style "backgroundColor" ...`, already against elm/html convention) stop
working with the patch.

Design rationale, the checked/unchecked boundary, and open extensions are
in [css-blocks-design.md](css-blocks-design.md).
