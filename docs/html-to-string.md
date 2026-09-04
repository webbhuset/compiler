# HTML to String

`VirtualDom.toString` renders a node as HTML text, so a server can serve a
page instead of a browser building it:

```elm
import VirtualDom as V

toString : Int -> V.Node msg -> String
```

The `Int` is how many spaces each level of nesting is indented by.

```elm
V.toString 0 (Html.p [] [ Html.text "Hello!" ])
--> "<p>Hello!</p>"

V.toString 2 (Html.p [] [ Html.b [] [ Html.text "Hi" ], Html.text "!" ])
--> "<p>\n  <b>Hi</b>\n  !\n</p>"
```

**Zero means no added whitespace at all**, and it is the only setting that
cannot change what the page means: whitespace between inline elements is
text the browser renders, so indenting `<span>a</span><span>b</span>` puts a
space between the two words. Use `0` to serve, and a larger number when you
want to read the output while developing.

Indenting stops where whitespace matters anyway: the content of `pre`,
`textarea`, `script` and `style` is written exactly as given, and an element
whose only child is text stays on one line, so `<title>` never grows
whitespace it did not ask for.

This needs the forked elm/virtual-dom `1.100.502`
([patch](patches/elm-virtual-dom-to-string.patch)), which `elm init` pins.

## Documents: doctype and comments

Two node kinds exist for writing a whole document:

```elm
V.doctype : String -> V.Node msg
V.comment : String -> V.Node msg
```

A document is a doctype followed by an `<html>` element — two nodes, so
render them one after the other:

```elm
page : String
page =
    String.concat
        [ V.toString 0 (V.doctype "html")
        , V.toString 0 <|
            Html.node "html"
                [ A.attribute "lang" "sv" ]
                [ Html.node "head"
                    []
                    [ Html.node "title" [] [ Html.text "Hej" ]
                    , Html.node "meta" [ A.attribute "charset" "utf-8" ] []
                    , Html.node "script" [ A.src "/app.js" ] []
                    ]
                , Html.node "body" [] [ V.comment " ssr ", view model ]
                ]
        ]
```

```html
<!DOCTYPE html><html lang="sv"><head><title>Hej</title>...
```

In a browser a comment is a real comment node: it diffs like any other
node, and `virtualize` keeps the comments in server-rendered markup, so an
app taking over a pre-rendered page sees them where they are instead of
replacing them. A doctype has no DOM node it could be, so there it renders
as an empty text node and never patches.

## What is written, and what is not

The output is the tree as written, not as a browser would be allowed to see
it:

| in the view | `toString` | in the DOM |
| --- | --- | --- |
| `node "script" [] [...]` | `<script>...</script>` | `<p>...</p>` |
| `attribute "onclick" "go()"` | `onclick="go()"` | `data-onclick="go()"` |
| `attribute "formaction" "/x"` | `formaction="/x"` | `data-formaction="/x"` |

Rewriting a `script` tag or an `on*` attribute name is a defense against
injecting into *this* document, so in this fork it happens on the way into
the DOM (`_VirtualDom_render` and `_VirtualDom_applyAttrs`) rather than
where the node is built. The browser is defended exactly as before; the
node keeps what you wrote, and `toString` renders it.

**This makes escaping your problem where it used to be handled for you.**
`attribute "onclick" userInput` now reaches your output. Text and attribute
values are escaped (`&`, `<`, `>`, and `"` in attributes), but an attribute
*name* built from user input is yours to check. Note that
`Html.Attributes.href`, `src` and `action` still neutralize a `javascript:`
URI, because elm/html does that where the attribute is built, before any of
this sees it.

Three things cannot be written down and are left out:

- **Event handlers.** A handler holds a decoder and functions.
- **Custom nodes.** A custom node renders itself by building a real DOM
  node, and there is no DOM to ask. `elm-explorations/webgl` uses these.
- **`innerHTML` and `outerHTML` properties**, which set markup rather than
  an attribute. (elm/virtual-dom already renames them to `data-*` where the
  property is built.)

`lazy` is forced and rendered, `map` renders straight through, and a keyed
node renders its children in order.

## Attributes and properties

Elm has both properties (`className`, set in JS) and attributes (`class`,
set in HTML), and HTML text only has attributes, so properties are
translated: `className` becomes `class`, `htmlFor` becomes `for`,
`acceptCharset` becomes `accept-charset`, and so on for the couple of dozen
names that differ. A property whose name already matches its attribute is
used as it is.

- A boolean property is an HTML boolean attribute: `checked=""` when `True`,
  absent when `False`.
- `class` given both as an attribute and as the `className` property is
  merged into one attribute.
- Styles become one `style` attribute, with the property names as written.
  Since this fork sets styles with `setProperty` (see
  [css-blocks.md](css-blocks.md)), a camelCase name like `backgroundColor`
  does not work in either output; write `background-color`.
- An HTML attribute name is written lowercase, since that is what the DOM
  does with it — elm/html's `tabindex` sets the attribute `tabIndex`, and
  both outputs say `tabindex`. Inside a namespace the case is kept, so SVG
  attributes like `viewBox` survive.
- A namespaced element renders with its plain tag name and no `xmlns`,
  which is what inline SVG in an HTML document wants. Serving standalone
  XML would need the attribute added by hand.
- Void elements (`br`, `img`, `input`, `meta`, …) have no closing tag.
