# Widen — Design Proposal

**Status: implemented on the `widen` branch, as designed below with the
`Basics.widen` surface. Needs the forked elm/core 1.0.8
([patch](patches/elm-core-widen.patch)). User-facing docs are in
[structural-variants.md](structural-variants.md#widening); this document
keeps the rationale and the alternatives considered.**

Structural variants can *shrink* along a pipeline: a `case` subtracts the
tags it handles, and `removeLoading : r -> [ r | Loading ] -> r` is
expressible today. The opposite direction — using a value at a row with
*more* tags than its type — is not, because unification only ever equates
rows. `widen` is that missing direction: a checked, zero-cost coercion from
a variant row into any row that includes it.

```elm
widen : a -> a      -- at runtime: the identity function
```

The value is untouched; a tag value is already a valid inhabitant of every
union containing its tag, since the runtime representation carries the
canonical tag name and nothing else. Only the type checker needs
convincing, so `widen` compiles to nothing.


## The problem

Rows widen implicitly in exactly one situation: a freshly constructed value
has an open row (`Success 3 : [ r | Success Int ]`), and open rows unify
into anything containing their tags. The friction starts when a value's
type has a *closed* or *rigid* row, which happens at every annotation
boundary:

```elm
type tag Loading
type tag Success value
type tag Failure error


type alias Model =
    { status : [ Loading, Success Data ] }      -- necessarily closed


report : [ Loading, Success Data, Failure String ] -> String
report status = ...


view : Model -> Html msg
view model =
    text (report model.status)
```

```
This `status` value is a:

    [ Loading, Success Data ]

But `report` needs the 1st argument to be:

    [ Failure String, Loading, Success Data ]
```

A record field cannot hold an open row without parameterizing the whole
alias (`Model r` — the parameter infects every signature that touches the
model), so stored variants are closed. A closed row does not unify with a
strictly larger closed row: unification is symmetric, and this direction is
subtyping. Today the only way through is to re-match and rebuild:

```elm
view model =
    text <|
        report <|
            case model.status of
                Loading -> Loading
                Success d -> Success d
```

This works — reconstruction produces fresh open rows — but it is O(tags) of
boilerplate that must grow every time the union grows, at every such
boundary. With `widen` it is one word, and it stays correct as the union
evolves:

```elm
view model =
    text (report (widen model.status))
```


## Typing rule

```
e : [ ρ₁ ]         ρ₁ ⊆ ρ₂
─────────────────────────────
widen e : [ ρ₂ ]
```

Row inclusion `ρ₁ ⊆ ρ₂` is checked on the solved rows and means:

1. **Width** — every tag of ρ₁ appears in ρ₂.
2. **Payloads unify exactly** — argument types of shared tags are unified,
   not recursively included. `[ Wrap [ A ] ] ⊆ [ Wrap [ A, B ] ]` is
   rejected even though it would be representation-sound, because depth
   inclusion needs variance tracking (see Restrictions).
3. **Tails** — ρ₁'s tail is either **closed** or **the same variable** as
   ρ₂'s tail. Two different abstract tails never pass: ρ₁'s unknown
   remainder could contain tags ρ₂ lacks.

If ρ₁'s tail is still an unconstrained flex variable when the check runs,
the checker falls back to ordinary unification of the two rows — so
`widen` on a freshly constructed value behaves exactly as if `widen` were
not there, instead of failing as ambiguous.

The "same tail" case is what makes widening compose with subtraction. A
narrowed catch-all has type `tail`; widening it into `[ tail | ... ]`
passes rule 3 trivially:

```
tail  ⊆  [ tail | Success Int, Loading ]      -- same remainder, tags added
```


## Use cases

### Stored values meeting wider consumers

The `Model` example above. This is the everyday case: values parked in
records, dictionaries, or messages get closed rows from their annotations,
and any consumer that also handles *other* tags needs the coercion. One
`widen` at the call site, no rebuild function to maintain.

### Pipelines that add outcomes

Row subtraction already supports pipelines where each step *discharges* a
tag (the `Displayable` example in structural-variants.md). The dual — a
step that can *introduce* a new failure — needs `widen` for its
pass-through branch:

```elm
type tag Valid value
type tag TooShort
type tag BadChar


checkLength : [ r | Valid String ] -> [ r | Valid String, TooShort ]
checkLength result =
    case result of
        Valid s ->
            if String.length s < 8 then TooShort else Valid s

        rest ->
            widen rest


checkChars : [ r | Valid String ] -> [ r | Valid String, BadChar ]
checkChars result =
    case result of
        Valid s ->
            if String.all Char.isAlphaNum s then Valid s else BadChar

        rest ->
            widen rest


validate : String -> [ r | Valid String, TooShort, BadChar ]
validate input =
    Valid input
        |> checkLength
        |> checkChars
```

Each step names only the tag it adds; the caller's other tags flow through
the abstract `r`. Without `widen`, each step must either enumerate every
tag it forwards (impossible — `r` is abstract) or declare its input row to
already contain its own output tag, which forces every step to know about
every other step's failures — exactly the central-registry coupling
structural variants exist to avoid.

The narrowed catch-all combines with this: subtract on the way in, widen on
the way out —

```elm
type tag Retrying


retryable : [ r | Failure String ] -> [ r | Failure String, Retrying ]
retryable status =
    case status of
        Failure err ->
            if isTransient err then Retrying else Failure err

        rest ->
            widen rest      -- rest : r,  widened into [ r | Failure String, Retrying ]
```

### Merging values from different unions

Two values with different closed rows have no common type today, even when
a union of both is the obvious meaning:

```elm
initialStatus : Bool -> [ NotAsked, Loading ]      -- ILLEGAL today for
initialStatus eager =                              -- values built elsewhere
    if eager then cachedLoading else cachedNotAsked

-- with widen:
initialStatus eager =
    if eager then widen cachedLoading else widen cachedNotAsked
```

Closed tails are included in anything (rule 3), so both branches coerce
into the annotated union.

### Adapting between library vocabularies

Two packages that share tag declarations but handle different subsets can
be bridged without conversion functions: a value of package A's
`[ Timeout, NetworkError ]` passes into package B's
`[ Timeout, NetworkError, DecodeError ]` handler as `widen err`. The
adapter stays correct when B's union grows.


## Why it must be a primitive

The obvious library function is not typeable:

```elm
embed : [ r ] -> [ r | Loading ]      -- occurs error: r ~ [ r | Loading ]
```

A `Forall` type can only *equate* rows; "the same row plus these tags"
is an inclusion between two types, which Hindley–Milner cannot express.
Gaster–Jones row calculi ship embedding as a primitive for exactly this
reason. (The alternative that makes `embed` a real function is presence
polymorphism — a per-label presence variable on every row — which would
roughly double the row machinery and infect every printed type. Rejected
as disproportionate.)


## Why explicit rather than implicit

Implicit widening at every use site is subtyping: it replaces
unification-based inference with subsumption solving, losing principal
types and the current error quality. An explicit `widen` keeps inference
untouched — everywhere else rows still unify — and marks every place a
value crosses into a bigger union, the way OCaml marks coercions with
`:>`. One word of syntax at the boundary is the cost; grep-ability of
those boundaries is arguably a benefit.


## Surface design: a known function, not a keyword

`widen` should **not** be a reserved word. Unlike `tag` (which is
contextual after `type`), an expression-position soft keyword is
indistinguishable from application of a user function named `widen`.
Instead, follow the `Debug.log` precedent — a real function in the forked
elm/core that canonicalization recognizes:

```elm
-- in the forked elm/core, module Basics (auto-imported):
widen : a -> a
widen a =
    a
```

- A **direct application** `widen e` (resolved to `Basics.widen`) becomes a
  dedicated `Can.Widen` node with the row-inclusion typing rule.
- **First-class uses** — `List.map widen`, `f widen`, `widen` passed as an
  argument — are left as the plain function, typed `a -> a`. This is a
  sound degradation, not a trap: the identity typing is strictly weaker,
  never wrong. Anyone relying on widening through a higher-order call gets
  a normal type error at the call site.
- A user's own `widen` definition shadows the Basics one, exactly like
  shadowing `min` or `identity`; the special rule only fires for the
  Basics home.

Living in `Basics` makes it available unqualified everywhere, like
`identity` and `always`. Putting it in a new `Variant` module instead is
the conservative option if polluting `Basics` feels wrong — see Open
questions.


## Implementation sketch

- **Canonicalize**: in `Canonicalize.Expression`, when the function of a
  `Call` resolves to `Basics.widen` with exactly one argument, produce
  `Can.Widen arg` instead of the call. Zero parser changes.
- **Constrain** (`Type.Constrain.Expression`): for `Can.Widen inner`,
  create fresh variables `v₁`, `v₂`; constrain `inner` against `v₁`;
  equate `v₂` with the expected type; and emit a deferred obligation
  `(region, v₁, v₂)` instead of unifying them.
- **Solve**: obligations collect in the solver state and are verified
  right before generalization of the enclosing definition — the same
  timing as the existing occurs check, when both rows are as solved as
  they will get. The check reuses `Unify.gatherTags` on both sides,
  unifies shared payloads pairwise, and applies the tail rule, with the
  flex-tail fallback of full unification. Inclusion is **stable under
  instantiation** — "same tail variable" stays the same variable in every
  copy, closed stays closed, unified payloads stay equal — so checking
  once per definition is sound for all later uses.
- **Errors**: a dedicated report with both rows rendered and the specific
  failure named: the target is missing a tag; a shared tag's payload
  mismatches; or the rows have different abstract remainders (with a hint
  that the remainders must match or the source must be a closed row).
- **Optimize / codegen**: `Can.Widen inner` optimizes to `inner`; the node
  vanishes before `Opt`, so the JS backend and the `.elmo` format are
  untouched.
- **Nitpick**: recurse through the node; nothing else to check.


## Restrictions

- **Shallow.** No widening inside payloads: `[ Wrap [ A ] ]` does not
  widen to `[ Wrap [ A, B ] ]`. Depth inclusion is representation-sound
  for variant payloads but needs variance tracking (function arguments
  flip direction) — that is a subtyping checker, not a row check. Widen at
  the inner construction site, or rebuild the wrapper.
- **Variants only.** The record dual is *forgetting* fields, and that is
  not sound in Elm: the extra fields remain in the object, so `(==)` and
  `Debug.toString` would observe them. Record shrinking needs a real
  restriction operation plus lacks constraints — out of scope.
- **Tails must be closed or identical.** Widening between two unrelated
  abstract rows is exactly the unsound case and is always rejected.


## Alternatives considered

- **Reserved keyword `widen e`** — costs a word, breaks any code using the
  name, and buys nothing over the Basics function. Rejected.
- **Unary operator `~e`** — mechanically clean: `~` is outside Elm's binop
  character set (`+-/*=.<>:&|^?%!`, see `Parse.Symbol.binopCharSet`), so a
  fresh prefix symbol has no binary reading and avoids the whitespace
  ambiguity that makes prefix `-` special-cased in three places. One term
  parser case, and — the one substantive advantage — **no forked-core
  release**: the feature becomes compiler-only. Rejected as the primary
  design anyway: an operator cannot be piped (`x |> widen |> f` is how the
  boundary sites actually read), has no first-class form (`List.map widen`),
  is not greppable or searchable, and a novel unary operator cuts against
  Elm 0.19's removal of user operators. Recorded as the fallback if
  shipping a core release proves to be the blocker; other symbols surveyed
  (`...` spread-alike, `!`, `?`, `@`, `#`, `$`) all drag in misleading
  connotations.
- **Per-tag embed functions** (`embedLoading : ...`) — not typeable at
  all; see "Why it must be a primitive".
- **Implicit subsumption** — MLsub/MLstruct-style subtyping inference.
  Replaces the unifier; rejected.
- **Presence polymorphism** (Rémy rows) — makes `embed` a real function
  type and also solves depth inclusion, at the cost of a presence variable
  on every row label, new annotation syntax, and a rewrite of row
  unification, printing, and error diffing. Rejected as disproportionate
  to the ergonomic gap.
- **Do nothing** — the rebuild-`case` idiom covers closed rows and is the
  current recommendation until real usage shows the boilerplate recurring;
  but it cannot express pass-through expansion over an abstract tail at
  all (the pipeline use case), and rebuild functions silently accumulate
  maintenance debt as unions grow.


## Open questions

- **Home module**: `Basics.widen` (auto-imported, reads like a builtin) vs
  a new `Variant.widen` (keeps Basics untouched, qualified name documents
  itself). Leaning `Basics` for ergonomics; needs a forked-core release
  either way.
- **Unconstrained targets**: `let w = widen x` with no further use leaves
  ρ₂ fully flexible; the fallback unifies ρ₂ := ρ₁, so `w` behaves like
  `x`. Alternative: report an ambiguity error and require an annotation.
  The fallback seems friendlier and is never unsound.
- **Interaction with tier-2 recursion**: once recursive named tag unions
  exist, inclusion between two recursive unions should stay shallow
  (compare one unrolling; the tails are the named aliases and must match
  by rule 3). To revisit when recursion lands.
