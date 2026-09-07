# Comparable Newtypes

This fork extends the `comparable` typeclass to **opaque newtypes**: custom
types with exactly one constructor wrapping exactly one comparable value.
They can be used as `Dict` keys, put in `Set`s, sorted, and compared:

```elm
type Id
    = Id String


lookup : Id -> Dict Id User -> Maybe User
lookup id users =
    Dict.get id users
```

## What qualifies

A custom type is comparable when it has a single constructor with a single
argument, and the argument's type is itself comparable:

- `Int`, `Float`, `Char`, `String`
- tuples and `List`s of comparable values
- other comparable newtypes (transitively, across modules and packages),
  e.g. `type UserId = UserId Id`
- a type parameter, as long as it is comparable where the type is used

This works whether or not the constructor is exported — opaque types are
the intended use case. Everything else is unchanged: multi-constructor
types, records, and functions are still not comparable.

### Type parameters

A parameter matters only if the payload mentions it. That makes a phantom
type comparable for every argument, since the argument never reaches a
value:

```elm
type Id t
    = Id String


users : Dict (Id User) User
orders : Dict (Id Order) Order
```

When the payload does mention a parameter, the type is comparable exactly
when that argument is, the same rule as `List a`:

```elm
type Box a
    = Box a


Dict (Box Int) v            -- fine
Dict (Box (Int -> Int)) v   -- not comparable, same as List (Int -> Int)


sortBoxes : List (Box comparable) -> List (Box comparable)
sortBoxes =
    List.sort
```

With several parameters each is judged on its own: `type Pair a b = Pair
( a, Int )` needs `a` comparable and ignores `b`.

## Semantics

Two wrapped values compare exactly as their payloads compare. In
`--optimize` builds this is literally free: single-constructor
single-argument types are already compiled to their bare payload, so
`compare` never sees a wrapper. Dev builds unwrap at runtime.

## Runtime: patched elm/core

Dev-mode comparison needs a small patch to `elm/core`'s kernel, shipped
the same way as task ports: apply
[patches/elm-core-comparable-newtypes.patch](patches/elm-core-comparable-newtypes.patch)
to your elm/core fork and consume it via a
[git dependency](git-dependencies.md).

## Notes

- The interface file format changed to carry comparability information,
  so the first build after upgrading rebuilds caches (`elm-stuff` and the
  packages in `ELM_HOME`). This is automatic — corrupt-looking caches are
  simply rebuilt. Avoid switching back and forth between this fork and
  the official compiler on the same `ELM_HOME`, or they will keep
  invalidating each other's caches.
- `elm diff` does not know that changing a newtype's payload to something
  non-comparable breaks downstream `Dict` users; within a private package
  ecosystem, treat such a change as a major version bump yourself.
