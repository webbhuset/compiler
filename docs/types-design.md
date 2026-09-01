# Thinking about types

> **This is a design sketch, not what the compiler does today.** It describes a
> simplification we are considering, and assumes three changes that have not
> been made:
>
> 1. A `type` declaration has exactly **one** constructor, named after the
>    type, so the constructor is written implicitly: `type Person = { ... }`.
> 2. Multi-constructor types are gone. Every "one of these" is a row.
> 3. A row written in a `type` declaration **declares its tags**, so simple
>    cases need only one declaration.
>
> For what exists now, read [records-and-variants.md](records-and-variants.md);
> the mental model there is the same, but today a `type` may list many
> constructors, which is a fourth way of saying something this design says
> once.

Elm has a small number of ways to describe the shape of a value, and they can
look like a pile of unrelated features. They are not. **Every type is an answer
to three questions**, and once you can ask those three questions in order, you
can write any type in the language.

1. Does a value hold **all** of these, or **one** of them?
2. Is that **exactly** those, or **at least** those?
3. Does it have a **name of its own**, or is it just its shape?

The rest of this page is those three questions, one at a time.


## Question 1: all of them, or one of them?

Say you are describing a person. A person has a name **and** an age:

```elm
{ name : String, age : Int }
```

That is a **record**. Every value of that type carries both fields, always.

Now say you are describing what a form field contains. It holds a piece of text
**or** a number, never both:

```elm
[ Text String, Number Int ]
```

That is a **variant type**. Every value is one of the listed alternatives, and
the labels — `Text`, `Number` — are **tags** that say which one it is.

That "and" versus "or" is the entire difference, and it is worth getting a feel
for how much bigger "and" is. If a type has a `Bool` and an `Int`, it has (2 ×
however many `Int`s) possible values, because every combination of the two is a
value. If it has a `Bool` **or** an `Int`, it has (2 + however many `Int`s),
because each value is one or the other. People sometimes call these *product*
and *sum* types for that reason. You will not need the words.

A tuple is a record that uses positions instead of names, so `( String, Int )`
and `{ name : String, age : Int }` hold exactly the same information. Prefer
the record as soon as the positions stop being obvious.

### Making one

You build a record by writing it out, and read it with a dot:

```elm
alice : { name : String, age : Int }
alice =
    { name = "Alice", age = 30 }


alice.name          --> "Alice"
```

You build a variant by using one of its tags, and read it with a `case`:

```elm
field : [ Text String, Number Int ]
field =
    Text "hello"


describe : [ Text String, Number Int ] -> String
describe f =
    case f of
        Text s ->
            s

        Number n ->
            String.fromInt n
```

If you forget a branch, the compiler tells you which tag you missed. That is
not a special check bolted on — it falls out of the type, which is why it also
works for tags nested inside other tags.


## Question 2: exactly those, or at least those?

So far every type has been **closed**: exactly these fields, exactly these
tags. The other option is **open**: at least these, possibly more. You write it
with a type variable in front of a `|`, which is read as *the rest*:

```elm
{ r | name : String }          -- at least a name, maybe other fields
[ r | Text String ]            -- at least Text, maybe other tags
```

### Why you want this

Because a function should ask for what it needs and nothing more. This one
reads a name, so it asks for a name:

```elm
greet : { r | name : String } -> String
greet person =
    "Hello, " ++ person.name
```

and now it works for *any* record that has a `name`, whatever else it carries:

```elm
greet { name = "Alice", age = 30 }
greet { name = "Bob", email = "b@example.com", admin = True }
```

Without that, you would need one `greet` per record shape.

Variants get the same benefit from the other end. A function that only cares
about one tag can say so, as long as it has a fallback for the rest:

```elm
isText : [ r | Text String ] -> Bool
isText f =
    case f of
        Text _ ->
            True

        _ ->
            False
```

### The part that surprises everyone

Open records and open variants behave in **opposite** ways, and the rule is
short enough to memorise:

> `r` on the *left* of a function is free. On the *right* it is a promise.

For a record that promise is "I will produce fields I have never heard of",
which is impossible:

```elm
makePerson : String -> { r | name : String }     -- cannot be written
makePerson n =
    { name = n }                                 -- what about r?
```

The caller picks what `r` is, so `makePerson` would have to invent fields it
cannot know about. You can still *return* an open record if you were handed
one, because then the extra fields came from the caller:

```elm
rename : String -> { r | name : String } -> { r | name : String }
rename n person =
    { person | name = n }
```

For a variant the same promise is "you may ignore alternatives I never
produce", which costs nothing — so building an open variant is fine:

```elm
parse : String -> [ r | Text String, Number Int ]
parse s =
    case String.toInt s of
        Just n ->
            Number n

        Nothing ->
            Text s
```

Reading flips it back: to read a variant you must handle **every**
alternative, and `r` stands for ones you cannot name, so an open variant needs
a `_` branch.

|                    | open record `{ r \| name : String }` | open variant `[ r \| Text String ]` |
| ------------------ | ------------------------------------ | ----------------------------------- |
| **as an argument** | free — accepts extra fields          | needs a `_` branch                  |
| **as a result**    | impossible                           | free                                |

Which gives four habits worth forming:

- Functions that **read** a record take an open one.
- Functions that **build** a record return a closed one.
- Functions that **build** a variant return an open one.
- Functions that **read** a variant take a closed one, listing exactly what
  they handle — or an open one if they have a `_`.

### A closed type needs no variable

There is nothing left unnamed in a closed type, so there is nothing for a
variable to stand for. Write `{ name : String }`, not `{ r | name : String }`
with an unused `r` — the compiler will tell you off for the second.


## Question 3: a name of its own, or just a shape?

Everything so far has been **structural**: the type *is* its shape. Two people
who independently write `{ name : String, age : Int }` have written the same
type, and values flow freely between them. Same for `[ Text String, Number Int ]`.

Sometimes you want the opposite — a type that is distinct because you said so.
That is a **nominal** type, and you make one by giving a shape a name:

```elm
type Person =
    { name : String
    , age : Int
    }
```

This creates a type called `Person` **and** a constructor called `Person` that
wraps a record up into one:

```elm
alice : Person
alice =
    Person { name = "Alice", age = 30 }
```

A `Person` is not a record any more. To get at the fields you unwrap it, which
you can do right in the argument:

```elm
name : Person -> String
name (Person p) =
    p.name
```

The same works over a variant, and here the row can declare its own tags:

```elm
type Field =
    [ Text String, Number Int ]


describe : Field -> String
describe (Field f) =
    case f of
        Text s ->
            s

        Number n ->
            String.fromInt n
```

Unwrap once in the argument, then carry on as normal.

### Why bother

Four reasons, and if none of them apply, don't — a plain record or row is
simpler.

**To keep things apart.** `type Meters = Float` and `type Feet = Float` are
different types, so you cannot pass one where the other is wanted. A structural
`Float` would let you.

**To hide the inside.** If a module exposes `Person` but not the `Person`
constructor, nobody outside can build one or take one apart. That is how you
enforce an invariant: everything has to go through the functions you provide.

**To carry a type parameter that isn't in the shape.** `type Id a = String` is
a `String` that remembers what it identifies, so a `Id User` never gets passed
where an `Id Order` belongs.

**To be recursive.** Which needs its own section.


## Recursion

A `type alias` is a *substitution* — the compiler replaces the name with what
is on the right. So an alias can never refer to itself; substituting forever
never finishes:

```elm
type alias Person =
    { name : String, boss : Person }
```

```
This type alias is recursive, forming an infinite type!
```

That has nothing to do with records or variants; the same happens either way.

A `type` is different. It introduces a real name, and the compiler can refer to
the name without expanding it. So **anything recursive needs a `type`
somewhere**:

```elm
type Tree a =
    [ Leaf a, Node (Tree a) (Tree a) ]


size : Tree a -> Int
size (Tree t) =
    case t of
        Leaf _ ->
            1

        Node left right ->
            size left + size right
```

The wrapper costs nothing at runtime — a type with one constructor is unboxed
when you compile with `--optimize`.


## Sharing tags between types

Tags are declared on their own when you want more than one type to use them:

```elm
type tag NotFound path
type tag PermissionDenied path


type ReadError =
    [ NotFound String, PermissionDenied String ]
```

A tag belongs to the module that declares it — your `NotFound` and someone
else's are different tags even though they are spelled the same — but it is not
tied to any one type. That is what makes the open-variant style work for
errors, where each function names only the failures it can actually produce:

```elm
readFile : String -> Task [ r | NotFound String, PermissionDenied String ] Bytes
```

and a caller that handles `NotFound` is left with a type that no longer
mentions it.


## The three questions, together

| | closed | open |
| --- | --- | --- |
| **all of them** | `{ name : String }` | `{ r \| name : String }` |
| **one of them** | `[ Text String ]` | `[ r \| Text String ]` |

and any of those four can be given a name of its own by wrapping it in a
`type`. That is the whole system.

A short way to decide:

1. **Does a value hold everything, or one thing?** Everything → record.
   One thing → row.
2. **Does this function need exactly that shape, or at least it?** Reading a
   record, or writing a value that flows onward → open. Otherwise → closed.
3. **Does it need to be its own type?** Recursive, opaque, phantom, or just
   too easy to confuse with its neighbours → wrap it in a `type`. Otherwise
   leave it structural.


## Where to go next

- [records-and-variants.md](records-and-variants.md) — the same model against
  the compiler as it is today.
- [structural-variants.md](structural-variants.md) — the reference for rows:
  exhaustiveness, narrowing a row with a `case`, the restrictions, and the
  runtime representation.
