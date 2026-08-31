# Overloading by signature

One name can have many definitions, and the compiler picks the one whose
signature matches the type it is used at. There is no `class` and no
`instance`: a module declares a name abstract by writing a signature with no
body, and other modules define it by writing the same qualified name with a
concrete signature and a body.

```elm
module Ord exposing (Ordering(..))


type Ordering
    = Less
    | Same
    | More


-- abstract: the name exists, dispatches on `a`, has no body
Ord.compare : a -> a -> Ordering
```

```elm
module Card exposing (Card(..))

import Ord exposing (Ordering(..))


type Card
    = Card Int


Ord.compare : Card -> Card -> Ordering
Ord.compare (Card a) (Card b) =
    if a < b then
        Less

    else if a > b then
        More

    else
        Same
```

```elm
import Card exposing (Card(..))
import Ord


Ord.compare (Card 1) (Card 2)      --> Ord.Less
```

The qualified name in definition position is the whole ceremony. Nothing is
opened, nothing is imported beyond the module that declares the name, and the
use site reads like any other qualified call.


## Declaring a name abstract

An abstract declaration is a signature with no body, written qualified with the
declaring module's own name:

```elm
module Ord exposing (Ordering(..))

Ord.compare : a -> a -> Ordering
```

The first argument decides which definition a use site means, so it must be a
type variable. `Ord.compare : Int -> Int -> Ordering` is rejected: there is
only ever one `Int`, so there would be nothing to choose between.

The qualifier has to name the module the declaration is in. Writing
`Ord.compare : ...` inside module `Card` is an error, because then two modules
could both claim the name.

There is no default body. A default would be silently wrong for most types — an
`Same`-returning `compare` would break every sorted structure whose key type
forgot to define one — so a type with no definition is an error at the use
site, not a value that quietly misbehaves.


## Defining it for a type

A definition is the same qualified name with a concrete signature and a body:

```elm
Ord.compare : Card -> Card -> Ordering
Ord.compare (Card a) (Card b) =
    ...
```

Its first argument names the type it is for. That has to be a named type; a
type variable, a record or a tuple has no name to dispatch on.

Definitions can use the overload themselves, including on their own type:

```elm
type Suit
    = Clubs
    | Hearts


type Card
    = Card Suit Int


Ord.compare : Suit -> Suit -> Ordering
Ord.compare a b =
    Ord.compare (rank a) (rank b)


Ord.compare : Card -> Card -> Ordering
Ord.compare (Card s1 n1) (Card s2 n2) =
    case Ord.compare s1 s2 of
        Same ->
            Ord.compare n1 n2

        other ->
            other
```

Each call resolves separately: `Ord.compare (rank a) (rank b)` to the `Int`
definition, `Ord.compare s1 s2` to the `Suit` one, `Ord.compare n1 n2` to `Int`
again.

Definitions are not values. They have no name you can write, so they cannot be
exposed, passed around, or shadowed. Every route to one goes through the
overloaded name.


## Ownership

> A definition must live in the module that declares the **name**, or in the
> module that declares the **type** it dispatches on.

Both modules above satisfy it: `Card` owns `Card`, and `Ord` owns `compare`, so
`Ord` is where a definition for `Int` goes:

```elm
module Ord exposing (Ordering(..))

Ord.compare : a -> a -> Ordering


Ord.compare : Int -> Int -> Ordering
Ord.compare a b =
    ...
```

Without this rule any module could add `Ord.compare : Card -> Card ->
Ordering`, two modules could add different ones, and a sorted structure built
by one and read by the other would silently disagree about what `Card` means.
That is a data-structure soundness bug, not a style preference. The rule buys
exactly one definition per (name, type) pair in any program, with no orphan
rules and no warnings to configure.

The cost is that you cannot retroactively define a name for someone else's
type. The workaround is a wrapper type of your own, which you needed anyway to
get a *different* ordering than its owner chose.

Only nominal types can own a definition. `[ Red, Green ]` and `{ x : Float }`
have no home module, so there is nothing for the rule to bite on and they
cannot have definitions.


## What resolves and what does not

Resolution happens after type inference, so the compiler dispatches on the type
the solver actually settled on. Where that type is still open, the use site is
an error:

```elm
Ord.compare 7 7
```

```
-- AMBIGUOUS OVERLOAD ---------------------------------------------------------

I cannot tell which definition of Ord.compare this use needs:

23|     , describe (Ord.compare 7 7)
                    ^^^^^^^^^^^
The type it dispatches on came out as:

    number

which is not a specific enough type to pick a definition. Adding a type
annotation that says which type you mean should settle it.
```

A number literal is `number`, not `Int`, until something pins it down. An
annotation does:

```elm
seven : Int
seven =
    7


Ord.compare seven seven            --> Ord.Same
```

The same applies inside a polymorphic function. This does not compile, because
`a` is not a type:

```elm
sort : List a -> List a            -- cannot use Ord.compare
```

Writing the constraint down — `where Ord.compare : a -> a -> Ordering` — is how
that will be expressed, and is not implemented yet. Until then, an overloaded
name is only usable where the dispatch type is concrete.

When the type is concrete but has no definition, the error says which signature
to write and where:

```
-- NO DEFINITION --------------------------------------------------------------

There is no definition of Ord.compare for this type:

12|         out (Debug.toString (Ord.compare 1.5 2.5))
                                 ^^^^^^^^^^^
Using it here needs one with this signature:

    Ord.compare : Float -> Float -> Ord.Ordering

Add it to module Basics, or to module Ord.
```


## How it compiles

There is no dictionary and no runtime dispatch. Each use site is rewritten to a
direct call to one definition, so an overloaded call costs exactly what the
equivalent hand-written call costs, and unused definitions are dead code like
any other.

A definition becomes an ordinary top-level value in its module under a mangled
name, plus an entry in that module's overload table recording which type it is
for. The table travels in the module's interface, and each interface carries
the union of its imports' tables, so a table is complete for everything its
module can reach. Ownership is what makes that enough: a use site that
dispatches `Ord.compare` at `Card` necessarily depends on both `Ord` and
`Card`, and the definition lives in one of them.

Because interfaces changed shape, `elm-stuff` and `~/.elm` from an earlier
build have to be removed.


## Relationship to `comparable`

`comparable`, `number`, `appendable` and `compappend` are untouched. The
long-term plan is for them to become ordinary abstract names with ordinary
definitions, replacing a hard-coded lattice with something extensible, but that
needs `where` clauses first — `List.sort` cannot be written without them.


## Current limits

- **No `where` clauses.** An overloaded name can only be used where the
  dispatch type is concrete. This is the main thing missing.
- **Dispatch on the first argument only.** Return-type dispatch
  (`Decode.decode : Json -> Result Error a`) is not supported.
- **Ground kinds only.** An abstract name dispatches on a type, not on a type
  constructor, so there is no `Functor` and no `Monad`. `List.map`,
  `Task.map` and `Result.map` stay separate names.
- **No bundling.** You cannot require that `compare`, `min` and `max` are all
  defined together. In practice most of a class's methods are derivable from
  one primitive, and those become ordinary constrained functions once `where`
  clauses exist.
- **Not documented in package docs.** Abstract declarations and definitions do
  not appear in generated documentation.
