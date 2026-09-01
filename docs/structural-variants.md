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

If you want to know *why* you would reach for this rather than a custom type,
skip to [why this is useful](#why-this-is-useful), which builds a real
file-and-network pipeline. The sections before it are the mechanics.

For how tags fit alongside records and custom types, see
[types-design.md](types-design.md).


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
(proposed in [widen-design.md](widen-design.md), not implemented). Adding
tags to freshly constructed values needs nothing: construction is always
open.


## Why this is useful

The abstract version of the pitch is "a function can name exactly the tags it
produces". Here is what that buys in practice.

Errors are the case where it pays off most, because every operation fails in
its own particular way, and a caller almost never wants to handle all of them.
`System.File` is written this way — each operation's error row lists what that
operation can actually produce:

```elm
File.read  : String -> Task [ r | NotFound String, PermissionDenied String, IsADirectory String
                             , TooManyOpenFiles String, NameTooLong String, SymlinkLoop String ] String

File.write : String -> String -> Task [ r | NotFound String, PermissionDenied String, IsADirectory String
                                      , NoSpaceLeft String, ReadOnly String, TooManyOpenFiles String
                                      , NameTooLong String, SymlinkLoop String ] ()
```

Say a network module is written the same way:

```elm
module Net exposing (Timeout, NetworkDown, BadStatus, get)


type tag Timeout url
type tag NetworkDown url
type tag BadStatus code


get : String -> Task [ r | Timeout String, NetworkDown String, BadStatus Int ] String
```

### Chaining unions the rows

Read from a cache; if the file is not there, fetch it and write it down:

```elm
load url cache =
    File.read cache
        |> Task.onError
            (\err ->
                case err of
                    NotFound _ ->
                        Net.get url
                            |> Task.andThen
                                (\body ->
                                    File.write cache body
                                        |> Task.map (\_ -> body)
                                )

                    other ->
                        Task.fail other
            )
```

No annotation needed — the error type is inferred, and it is the union of
everything that can still go wrong:

```elm
load :
    String
    -> String
    ->
        Task
            [ r
            | NotFound String
            , PermissionDenied String
            , IsADirectory String
            , TooManyOpenFiles String
            , NameTooLong String
            , SymlinkLoop String
            , NoSpaceLeft String
            , ReadOnly String
            , Timeout String
            , NetworkDown String
            , BadStatus Int
            ]
            String
```

Three modules, three unrelated error vocabularies, no shared `Error` type to
declare and no wrapping constructors to write. With nominal types you would
need a `type LoadError = FileError File.Error | NetError Net.Error` and a
`Task.mapError` at every step to build it.

Look closely at `NotFound` in that row. It was handled — and it is still
there, because `File.write` can also produce it when the cache directory does
not exist. The row is telling the truth: the `NotFound` you can still get is
the one from writing, not the one from reading.

### Handling some errors, not all

Handling a tag and passing the rest along removes it. Treat a timeout and a
missing cache directory as "no cached copy" and let everything else through:

```elm
loadOrBlank url cache =
    load url cache
        |> Task.onError
            (\err ->
                case err of
                    Timeout _ ->
                        Task.succeed ""

                    NotFound _ ->
                        Task.succeed ""

                    other ->
                        Task.fail other
            )
```

`other` is bound at the row minus the two tags just matched, so the result
type shrinks on its own:

```elm
loadOrBlank :
    String
    -> String
    ->
        Task
            [ r
            | PermissionDenied String
            , IsADirectory String
            , TooManyOpenFiles String
            , NameTooLong String
            , SymlinkLoop String
            , NoSpaceLeft String
            , ReadOnly String
            , NetworkDown String
            , BadStatus Int
            ]
            String
```

The caller now cannot handle `Timeout`, because it can no longer happen. That
is the part a nominal error type cannot express: `LoadError` stays `LoadError`
however many of its cases you have already dealt with, so every caller keeps
re-handling cases that are dead, or reaches for a `_ ->` that silently absorbs
the ones that are not.

### The end of the chain proves you handled everything

Eventually something has to turn the remainder into a message. That function
takes a **closed** row — exactly what is left:

```elm
report :
    [ PermissionDenied String
    , IsADirectory String
    , TooManyOpenFiles String
    , NameTooLong String
    , SymlinkLoop String
    , NoSpaceLeft String
    , ReadOnly String
    , NetworkDown String
    , BadStatus Int
    ]
    -> String
report err =
    case err of
        NetworkDown url ->
            "network is down, could not reach " ++ url

        BadStatus code ->
            "the server said " ++ String.fromInt code

        PermissionDenied path ->
            "not allowed to touch " ++ path

        _ ->
            "could not use the cache"


run : String -> String -> Task String String
run url cache =
    loadOrBlank url cache
        |> Task.mapError report
```

Forget one and the compiler stops you at the join, naming it:

```
This function cannot handle the argument sent through the (|>) pipe:

The argument is:

    Task [ r | BadStatus Int, NetworkDown String, IsADirectory String, ... ] String

But (|>) is piping it to a function that expects:

    Task [ NetworkDown String, IsADirectory String, ... ] String

Hint: The BadStatus tag is not accepted here.

Note: Matching a tag and passing the rest along removes it from the row, so if
this value goes through other functions first, handling it in one of them is
often what you want.
```

Add an operation to `Net` that can fail a new way, and every pipeline that
does not handle it fails to compile, pointing at the exact tag. Nothing is
silently swallowed, and there is no central error type for the new case to be
added to.

One limit worth knowing: a formatter written over a closed row only accepts
that exact row. `System.Error.format` lists all thirteen file tags, so it
cannot be handed the nine that survive here — widening a closed row is not
expressible (see [widen-design.md](widen-design.md)). Handle the tags you care
about and finish with `_`, as `report` does.


## A second example: a display pipeline

The same shape works outside errors. Each step handles one tag and passes the
rest along, and an alias can name the recurring part — aliases may take row
parameters like any type variable:

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

- **Tag patterns can only appear where the variant row can be closed:** at
  the top of a `case` branch, inside another tag pattern, and inside a
  constructor argument whose declared type is a type *variable* — which
  covers the common wrappers:

  ```elm
  case result of
      Ok value -> ...
      Err (NotFound path) -> ...      -- the `e` of Result e a
      Err (Denied path) -> ...
  ```

  Exhaustiveness still holds through the nesting: leaving out `Denied` is a
  type error naming it. Tuples, lists, and constructor arguments of a fixed
  type are still rejected, because there is no row to close there and an
  unhandled tag would reach no branch at run time. Match the outer structure
  first and the tag in a second `case`.
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
