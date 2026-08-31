# Overloading by signature

One name can have many definitions, and the compiler picks the one whose
signature matches the type it is used at. There is no `class` and no
`instance`: a module declares a name with `abstract`, and other modules define
it by writing that name qualified, with a concrete signature and a body.

```elm
module Ord exposing (Ordering(..))


type Ordering
    = Less
    | Same
    | More


-- the name exists, dispatches on `a`, and has no body here
abstract compare : a -> a -> Ordering
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

One keyword and a qualified name in definition position is the whole ceremony.
Nothing is opened, nothing is imported beyond the module that declares the
name, and the use site reads like any other qualified call.


## Declaring a name abstract

An abstract declaration is a signature with no body, introduced by `abstract`:

```elm
module Ord exposing (Ordering(..))

abstract compare : a -> a -> Ordering
```

It declares the name in the module it is written in, which is what makes that
module the name's owner. `abstract` is contextual, like `port`, so a value
called `abstract` still works.

The first argument decides which definition a use site means, so it must be a
type variable. `abstract compare : Int -> Int -> Ordering` is rejected: there
is only ever one `Int`, so there would be nothing to choose between.

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

Its first argument names the type it is for. That has to be a named type or a
tuple; a type variable or a record has no name to dispatch on.

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

abstract compare : a -> a -> Ordering


Ord.compare : Int -> Int -> Ordering
Ord.compare a b =
    ...
```

A definition stays qualified even in the declaring module, because there the
qualifier says which overloaded name is being defined rather than which module
to look in.

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
cannot have definitions. Tuples are the one structural exception: they are
treated as belonging to `Tuple`, so only the module that declares the name can
define for them, which is the same answer the rule would give.

A type alias is not a type, so it cannot own anything either. Dispatch sees
through aliases, which means

```elm
type alias Name = String

Ordering.compare : Name -> Name -> Order      -- rejected outside String or Ordering
```

is a definition for `String` and has to live where one for `String` would.
Without that, `Name` and `String` — the same type — could each carry a
different ordering, and which one ran would depend on how a signature happened
to spell the type.


## Operators

An operator dispatches when the function behind it does. Declare it in the
usual way, pointing at a function with a `where` clause:

```elm
infix non 4 (|<|) = lt


lt : a -> a -> Bool
    where Ordering.compare : a -> a -> Order
lt x y =
    Ordering.compare x y == LT
```

Then `Card 1 |<| Card 2` picks the `Card` definition, `[ Card 2 ] |>| [ Card 1 ]`
builds the `List` one, and an operator used on a type variable asks for the same
clause a call would.

Two things limit this, and neither comes from overloading. `infix` declarations
are only allowed in kernel packages, which for this compiler means elm/* and
anything reached through `git-dependencies`; and `<`, `>`, `<=` and `>=` belong
to `Basics`, which every module imports openly, so only elm/core can give those
a new meaning. Making `<` itself dispatch means changing `Basics` to declare
`compare` abstract and derive the comparisons from it — at which point
`comparable` has nothing left to do.


## `where` clauses

A function that uses an overload on one of its own type variables says so,
because otherwise its signature would be a lie — it does not really work for
every `a`:

```elm
smallest : a -> a -> Ordering
    where Ord.compare : a -> a -> Ordering
smallest x y =
    Ord.compare x y
```

The clause is a signature, which is a concept the language already has. It says
nothing new about `Ord.compare`: the type has to be exactly the abstract
signature with its own variable replaced by the one you need it at, and the
compiler checks that. Several clauses stack, one `where` line each.

A definition can have them too, which is how an overload is given for a
container:

```elm
Ord.compare : List a -> List a -> Ordering
    where Ord.compare : a -> a -> Ordering
Ord.compare xs ys =
    case ( xs, ys ) of
        ( [], [] ) -> Same
        ( [], _ ) -> Less
        ( _, [] ) -> More
        ( x :: xrest, y :: yrest ) ->
            case Ord.compare x y of
                Same -> Ord.compare xrest yrest
                other -> other
```

Then `Ord.compare [ [ card ] ] [ [ card ] ]` works: the use resolves to the
`List` definition, which needs `Ord.compare` at `List Card`, which resolves to
the `List` definition again, which needs it at `Card`.

Nothing about this is visible in the type. `smallest` is a two argument
function, `List.sort` would be a one argument function, and a `where` clause
neither adds a parameter you can see nor changes what the value can be passed
to.


## What resolves and what does not

Resolution happens after type inference, so the compiler dispatches on the type
the solver actually settled on. Where that type is a variable with no clause
for it, the error is the line to add:

```
-- MISSING WHERE CLAUSE -------------------------------------------------------

This needs Ord.compare on `a`, which could be any type:

22|     Ord.compare x x
        ^^^^^^^^^^^
So the signature above has to say that it needs it, by adding this line under
it:

    where Ord.compare : a -> a -> Ord.Ordering
```

Where the type is still open, the use site is an error too:

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

When the type is concrete but has no definition, the error says where to put
one:

```
-- NO DEFINITION --------------------------------------------------------------

There is no definition of Ord.compare for this type:

12|         out (Debug.toString (Ord.compare 1.5 2.5))
                                 ^^^^^^^^^^^
    Float

Add one in module Basics, or in module Ord.
```


## How it compiles

Where the type is known, there is no dictionary and no runtime dispatch: the
use site becomes a direct call to one definition, so it costs exactly what the
equivalent hand-written call costs.

A `where` clause becomes one hidden leading parameter, and a reference to a
constrained value is applied to whatever its caller resolved. `smallest` above
compiles to a three argument function, and `smallest x y` at `Card` compiles to
`smallest(compareCard, x, y)`. Passing the argument is the only cost, and it is
paid only where a type variable made the choice genuinely late; a call at a
known type still resolves to a definition, and a chain of them is built once at
the outermost call rather than looked up as the program runs.

Definitions become ordinary top-level values in their module under mangled
names, plus an entry in that module's overload table recording which type each
one is for and what it needs in turn. The table travels in the module's
interface, and each interface carries the union of its imports' tables, so a
table is complete for everything its module can reach. Ownership is what makes
that enough: a use site that dispatches `Ord.compare` at `Card` necessarily
depends on both `Ord` and `Card`, and the definition lives in one of them.

Unused definitions are dead code like any other, though a definition reached
only through a `where` parameter is kept whenever the function that takes it
is.

Because interfaces changed shape, `elm-stuff` and `~/.elm` from an earlier
build have to be removed.


## Relationship to `comparable`

`comparable`, `number`, `appendable` and `compappend` are untouched. They could
now become ordinary abstract names with ordinary definitions, replacing a
hard-coded lattice with something extensible, but that is a change to elm/core
rather than to the compiler and has not been made.

`comparable` cannot be written as a definition, and deliberately so:

```elm
Ordering.compare : comparable -> comparable -> Order    -- rejected
```

A definition is chosen by the head constructor of its first argument, and
`comparable` is a type variable, not a type. Letting it stand for "every type
the checker happens to call comparable" would make it a default that overlaps
every real definition, and then which one wins at `Int` would need an
overlap rule. Write the definitions out instead — `Int`, `Float`, `Char`,
`String`, `List a` and tuples cover exactly what `comparable` covers, and
unlike `comparable` the list is open.


## Current limits

- **Top level definitions only.** A `let` definition cannot have `where`
  clauses, because it would have to take them as extra arguments and nothing
  passes them. Move it out, or use the overload at a specific type.
- **Clauses are not inferred.** The compiler prints the clause you need, but
  will not add it for you, and a definition with no annotation cannot use an
  overload on a type variable at all.
- **Dispatch on the first argument only.** Return-type dispatch
  (`Decode.decode : Json -> Result Error a`) is not supported.
- **Ground kinds only.** An abstract name dispatches on a type, not on a type
  constructor, so there is no `Functor` and no `Monad`. `List.map`,
  `Task.map` and `Result.map` stay separate names.
- **No bundling.** You cannot require that `compare`, `min` and `max` are all
  defined together. In practice most of a class's methods are derivable from
  one primitive, and those are ordinary functions with a `where` clause.
- **Not documented in package docs.** Abstract declarations, definitions and
  `where` clauses do not appear in generated documentation, so a constrained
  value's published type does not yet say what it needs.
