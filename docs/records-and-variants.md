# Records and variants

Elm now has four ways to describe the shape of a value, and it is easy to feel
like there are four unrelated features to learn. There are not. There are two
questions, and every one of these types is an answer to both.

**Does a value carry all of these, or one of them?**

```elm
{ flag : Bool, count : Int }     -- a Bool AND an Int, always both
[ Flag Bool, Count Int ]         -- a Bool OR an Int, never both
```

**Are those exactly the ones, or at least them?**

```elm
{ flag : Bool, count : Int }         -- exactly these two fields
{ r | flag : Bool, count : Int }     -- these two fields, and maybe more

[ Flag Bool, Count Int ]             -- exactly these two tags
[ r | Flag Bool, Count Int ]         -- these two tags, and maybe more
```

Crossing them gives the four:

|                | exactly these                | at least these                   |
| -------------- | ---------------------------- | -------------------------------- |
| **all of them** | `{ flag : Bool }`            | `{ r \| flag : Bool }`           |
| **one of them** | `[ Flag Bool ]`              | `[ r \| Flag Bool ]`             |

The `r` is an ordinary type variable, and it means the same thing in both
rows: *the part I am not naming here*.

If you have met the words before, "all of them" is a **product** and "one of
them" is a **sum**. The names come from counting: a record with a `Bool` and an
`Int` has (2 × however many Ints) possible values, because every combination is
one; a variant with a `Bool` and an `Int` has (2 + however many Ints), because
each value is one or the other. You will not need the words again.


## All of them, or one of them

A record is "all of them" with names. A tuple is the same idea with positions
instead of names, which is why `( Bool, Int )` and `{ first : Bool, second :
Int }` hold exactly the same information.

A variant type is "one of them". Elm has had one form of this since the
beginning — a custom type:

```elm
type Status
    = Loading
    | Success Int
    | Failure String
```

and now has a second, written with tags declared on their own:

```elm
type tag Loading
type tag Success value
type tag Failure error


[ Loading, Success Int, Failure String ]
```

Both say "one of these three". The difference is not what they mean but who
owns them, which is the subject of a later section.


## Exactly these, or at least them

The point of "at least" is that a function should be able to say what it needs
and nothing more.

```elm
name : { r | one : String } -> String
name rec =
    rec.one
```

`name` reads one field, so it asks for one field. Any record that has a `one`
can be passed to it, whatever else it carries:

```elm
big : { one : String, two : Int, three : Bool }
big =
    { one = "a", two = 1, three = True }


name big        -- fine
```

Variants work the same way from the outside:

```elm
wants : [ r | One String ] -> String
wants s =
    case s of
        One a ->
            a

        _ ->
            "other"


closed : [ One String, Three Int ]
closed =
    One "hi"


wants closed    -- fine
```

Both functions accept more than they named. That is all "open" buys you, and
it is worth having: without it, `name` would have to be written once per record
shape.


## The part that trips people up

Open records and open variants behave in opposite ways, and it is worth
knowing why, because the rule is short.

**An open record is easy to accept and impossible to build.**

```elm
type alias OpenRecord r =
    { r | one : String, two : Int }


fn10 : String -> OpenRecord r
fn10 s =
    { one = s, two = 0 }
```

```
Something is off with the body of the `fn10` definition:

7|     { one = s, two = 0 }
       ^^^^^^^^^^^^^^^^^^^^
The body is a record of type:

    { one : String, two : number }

But the type annotation on `fn10` says it should be:

    OpenRecord r
```

The annotation promised a record with `one`, `two`, **and whatever `r` turns
out to be**. The caller chooses `r`, so `fn10` would have to produce fields it
has never heard of. There is no way to write that, and there should not be.

You can still return one if you were *given* one, because then the extra fields
came from the caller:

```elm
setOne : String -> OpenRecord r -> OpenRecord r
setOne name rec =
    { rec | one = name }
```

**An open variant is the other way round: easy to build, and impossible to read
without a fallback.**

```elm
type alias OpenSum r =
    [ r | One String, Two Int ]


fn2 : Bool -> OpenSum r
fn2 b =
    if b then
        One "String"

    else
        Two 4
```

This compiles. Building a variant means producing *one* alternative, and `One`
and `Two` are both among the ones promised. The `r` stands for alternatives the
caller is willing to see but this function never produces, which costs it
nothing.

Reading flips it:

```elm
consume : [ r | One String, Two Int ] -> String
consume s =
    case s of
        One a -> a
        Two n -> String.fromInt n
```

```
The patterns in this `case` do not cover all the possible variant tags:

The branches cover variants of type:

    [ One a, Two b ]

But the expression between `case` and `of` is:

    [ r | One String, Two Int ]

Add branches until all the tags are covered, or add a final `_` branch to
handle everything else.
```

The `r` may be tags this function has never heard of, so it cannot claim to
have handled them.

The whole thing in one line: **`r` on the left of a function is free; on the
right it is a promise.** For a record that promise is "I will produce fields I
do not know about", which is impossible. For a variant it is "you may ignore
alternatives I never produce", which is free.

|                     | open record `{ r \| one : String }` | open variant `[ r \| One String ]` |
| ------------------- | ----------------------------------- | ---------------------------------- |
| **as an argument**  | free: accepts extra fields          | needs a `_` branch                 |
| **as a result**     | impossible                          | free                               |

Which gives the rule of thumb worth memorising:

- Functions that **build** a variant should return an open row.
- Functions that **consume** a variant should take a closed row, listing
  exactly what they handle — or an open row if they have a `_`.
- Functions that **read** a record should take an open row.
- Functions that **build** a record should return a closed one.


## Why "closed" needs no type variable

A closed row is just a row with nothing left unnamed, so there is nothing for
a variable to stand for:

```elm
type alias ClosedRecord r =
    { one : String, two : Int }
```

```
Type alias `ClosedRecord` does not use the `r` type variable.
```

Write `type alias ClosedRecord = { one : String, two : Int }` instead. The same
goes for `[ One String, Two Int ]`.


## Recursion

A common way to hear the rule is "only nominal types can be recursive", but
that is not quite it, and the real rule is easier to remember. It has nothing
to do with sums:

```elm
type alias Person =
    { name : String, boss : Person }
```

```
This type alias is recursive, forming an infinite type!
```

A `type alias` is a substitution — the compiler replaces the name with its
right-hand side. Substituting a name that contains itself never finishes, so no
alias can be recursive, record or variant.

A `type` declaration is different: it introduces a *name*, and the compiler can
refer to the name without expanding it. So anything recursive needs one
somewhere. For tags, that means a one-constructor wrapper:

```elm
type tag Node left right
type tag Leaf a


type Tree a
    = Tree [ Node (Tree a) (Tree a), Leaf a ]


leaf : Int -> Tree Int
leaf n =
    Tree (Leaf n)
```

The wrapper costs nothing at runtime — a type with one constructor taking one
argument is unboxed under `--optimize`.


## Nominal or structural

The remaining choice is who owns the type.

A custom type is **nominal**: `type Status = Loading | Success Int` creates a
name that belongs to its module. Two modules that both write that declaration
have two unrelated types, and a value of one is not a value of the other.

A tag row is **structural**: `[ Loading, Success Int ]` is the same type
wherever it is written, and no module owns it. (The individual *tags* are still
owned — a tag's identity is its declaring module plus its name — but the row
built from them is not.)

Reach for a custom type when the set of alternatives is fixed and the module
should decide what they are: `Maybe`, `Result`, a `Msg` type. Reach for tags
when different functions care about different subsets — errors above all, where
the value of `[ r | NotFound String, PermissionDenied String ]` is that each
operation names only what it can actually fail with, and handling one removes
it from what the caller sees.

Two practical consequences:

- Recursion needs a nominal type, as above.
- Only a type with an owner can carry an overload definition, so a custom type
  or a row carrying exactly one tag can; a row carrying several cannot, unless
  you wrap it. See [overloading.md](overloading.md).


## What is not there

Rows can be extended in a type, but there is no way to write "some record,
contents unknown":

```elm
addThree : { r } -> { r | three : Bool }
```

```
I am partway through parsing a record type, but I got stuck here:

addThree : { r } -> { r | three : Bool }
              ^
I just saw a field name, so I was expecting to see a colon next.
```

A record type must name at least one field. And there is no operation that
adds or removes a field from a record's *type*, so neither `addThree` nor
`removeThree : { r | three : a } -> { r }` can be written, however they are
spelled.

Variants are better served, but only in one direction:

|                          | records          | variants                                     |
| ------------------------ | ---------------- | -------------------------------------------- |
| **narrowing** (removing) | not expressible  | a `case` with a catch-all — see [row subtraction](structural-variants.md#row-subtraction) |
| **widening** (adding)    | not expressible  | free when constructing; not for an abstract remainder |

Variants got narrowing because a `case` already tells the compiler which tags
are gone, so the type can say so without new syntax. Records have no equivalent
moment. Widening an abstract remainder — turning a `[ r | Success Int ]` into a
`[ r | Success Int, Loading ]` — would need an explicit coercion, sketched in
[widen-design.md](widen-design.md) and not implemented.


## Where to go next

[structural-variants.md](structural-variants.md) is the reference: declaring
tags, exhaustiveness, row subtraction, a worked pipeline example, the
restrictions, and the runtime representation.

[types-design.md](types-design.md) tells the same story from scratch, for
someone new to Elm, against a proposed simplification in which a `type` always
has exactly one constructor and every "one of these" is a row.
