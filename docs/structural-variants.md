# Structural variants

Structural variants are anonymous sum types: the dual of Elm's extensible
records. Where a record type lists the fields a value *has*, a variant type
lists the tags a value *can be*:

```elm
type tag Loading
type tag Success value
type tag Failure error


state : Int -> [ r | Loading, Success Int ]
state n =
    if n > 0 then
        Success n

    else
        Loading


describe : [ Loading, Success Int, Failure String ] -> String
describe s =
    case s of
        Loading ->
            "loading"

        Success n ->
            "got " ++ String.fromInt n

        Failure err ->
            "failed: " ++ err
```

Unlike custom types, two functions can accept overlapping unions of the same
tags without a shared type declaration, and a `case` can require exactly the
tags it handles.


## Declaring tags

Tags are declared at the top level with `type tag` (`tag` is contextual, like
the `alias` in `type alias`, so it is not
reserved):

```elm
type tag Loading                 -- no arguments
type tag Success value           -- one argument
type tag Pair first second       -- two arguments
```

The lowercase names are type parameters, one per argument. They must be
distinct. A declaration creates a *tag*, usable as a constructor function, in
patterns, and in variant types:

```elm
Success : value -> [ r | Success value ]
```

**Tags are canonical.** A tag's identity is its declaring module plus its
name, like everything else in Elm. `Maybe.Just` and your local `Just` are
*different tags* even though they share a spelling — they can even coexist in
one variant type. Tags are exported with `exposing (Success)` and imported
like constructors; there is no `(..)` form. Two packages share a tag only by
importing it from a common module.


## Variant types

- `[ Loading, Success Int ]` — a **closed** row: exactly these tags.
- `[ r | Loading, Success Int ]` — an **open** row: at least these tags;
  `r` is a type variable like a record extension.
- `[]` — the empty row: a type with no values (like `Never`).
- Tags from other modules can be qualified: `[ Tags.Pending, Pending ]`.

Rules of thumb:

- **Constructing functions** should return an *open* row, so the result can
  flow anywhere the tags are accepted. This is also what inference produces
  when you omit the annotation.
- **Consuming functions** take a *closed* row listing what they handle, or an
  open row if they have a `_` fallback:

```elm
isLoading : [ r | Loading ] -> Bool
isLoading s =
    case s of
        Loading ->
            True

        _ ->
            False
```


## Exhaustiveness

Exhaustiveness falls out of the type system: a `case` without a `_` branch
*closes* the scrutinee's row to exactly the matched tags. An unhandled tag is
a type error at the `case`:

```
The patterns in this `case` do not cover all the possible variant tags:

The branches cover variants of type:

    [ A, B ]

But the expression between `case` and `of` is:

    [ A, B, C ]

Hint: It looks like the C tag is not handled.
```

This applies recursively to tags nested in other tag patterns
(`Wrap (Ok value)`), and to `let` destructuring and function-argument
patterns, which close the row to a single tag (they must be irrefutable).


## Row subtraction

When a `case` ends with a catch-all *variable*, that variable is bound at the
scrutinee's row **minus** the tags matched by the earlier branches — at
runtime it can never hold one of those tags, so the type says so:

```elm
removeLoading : r -> [ r | Loading ] -> r
removeLoading whenLoading status =
    case status of
        Loading ->
            whenLoading

        other ->
            other          -- other : r, the row WITHOUT Loading
```

This type is also what inference produces without the annotation. Reusing the
tail variable `r` bare as the result type is the idiom for subtraction, and
it composes: instantiating `r` derives row-changing functions that cannot be
written directly:

```elm
failLoading : [ f | Failure String, Loading ] -> [ f | Failure String ]
failLoading =
    removeLoading (Failure "Loading")
```

Closed rows subtract the same way, without rebuilding the other branches:

```elm
compact : [ Success Int, Failure String, Loading ] -> [ Success Int, Failure String ]
compact s =
    case s of
        Loading ->
            Failure "was loading"

        other ->
            other
```

Two rules govern what gets subtracted:

- Only tags matched **irrefutably** are removed. `Wrap (Ok n) -> ...` does
  not consume the whole `Wrap` tag — a `Wrap (Err e)` value still reaches the
  catch-all — so `Wrap` stays in the catch-all's type.
- The catch-all must be the **final** branch, with only tag patterns before
  it. A `_` wildcard binds nothing, so nothing is narrowed there.

The reverse direction — *adding* tags to an abstract remainder, as in
`[ r | Success Int ] -> [ r | Success Int, Loading ]` with a pass-through
branch — is not expressible; it would need an explicit `widen` coercion
(not implemented). Adding tags to freshly constructed values needs nothing:
construction is always open.


## A worked example

Row subtraction composes into pipelines where each step handles one tag and
passes the rest along, and the final step's closed row proves every tag was
handled somewhere. An alias can name the recurring shape — aliases may take
row parameters like any type variable:

```elm
type tag Done ok
type tag Error err
type tag Loading
type tag Empty
type tag Display message


type alias Displayable r =
    [ r | Display String ]


displayError : Displayable [ r | Error String ] -> Displayable r
displayError state =
    case state of
        Error err ->
            Display ("Error: " ++ err)

        r ->
            r


displayLoading : Displayable [ r | Loading ] -> Displayable r
displayLoading state =
    case state of
        Loading ->
            Display "Loading..."

        r ->
            r


display : [ Display String ] -> String
display (Display message) =
    message


test : List String
test =
    [ Loading
    , Error "Something went wrong"
    , Display "All good"
    ]
        |> List.map (displayError >> displayLoading >> display)
```

Each step subtracts the tag it converts: `displayError` turns `Error` into
`Display` and passes everything else through, so its result row no longer
contains `Error`. By the time values reach `display`, the pipeline has
discharged every tag except `Display String` — which is why `display` can
take a closed single-tag row and destructure it irrefutably in its argument
pattern.

The proof is in what happens when a tag slips in that no step handles. Add
`Empty` to the list and the pipe fails to type check:

```
This function cannot handle the argument sent through the (|>) pipe:

The argument is:

    List [ a | Display String, Empty, Error String, Loading ]

But (|>) is piping it to a function that expects:

    List (Displayable [ Error String, Loading ])

Hint: The Empty tag can still occur at this point, but it is not handled
here. Handle it before this point, or add it to the variant row that is
expected!
```

Adding a `displayEmpty` step to the pipeline fixes it — no central type
declaration to touch, no other function affected.


## Restrictions

- **Tag patterns cannot appear inside tuples, lists, records, or custom type
  constructor patterns.** Match the outer structure first, then the tag in a
  second `case`. (Tags nested in *other tag patterns* are fine.)
- **Recursion needs a named type.** `type alias Json = [ Num Float, Arr (List Json) ]`
  is still a recursive alias error; tie the knot with a custom type instead:

  ```elm
  type Tree = Tree [ Leaf, Node Tree Tree ]

  type tag Leaf
  type tag Node left right
  ```

- **No ports.** Variant values cannot flow through ports; convert to records
  or JSON first.
- Variant types are **not comparable** and cannot be `Dict` keys.
- Tags do not appear in `docs.json` (the format has no field for them), so
  package docs will not render them.
- `Debug.toString` shows the internal qualified tag name
  (`author/project:Module.Tag`), not the short name.


## Runtime representation

A tag value is the same object shape as a custom type value:
`{ $: "author/project:Module.Tag", a = ..., b = ... }`. The `$` field is the
fully qualified tag name (in both dev and `--optimize` builds), which is what
lets same-named tags from different modules coexist in one union. Structural
equality (`==`) works as usual.
