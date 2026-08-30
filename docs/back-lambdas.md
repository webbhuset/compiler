# Back-lambdas

A **back-lambda** is a lambda written with the arrow reversed, `\x <- ...`,
which binds its argument for the lines that *follow* it instead of nesting
them inside an expression. It gives callback-heavy code — decoders, tasks,
parsers, `Maybe`/`Result` chains — a flat, do-notation-like shape without
adding any new evaluation rules: it is pure syntax sugar for a lambda.

```elm
userDecoder : Decoder User
userDecoder =
    \id <- await (D.field "id" D.string)
    \firstname <- await (D.field "firstname" D.string)
    \lastname <- await (D.field "lastname" D.string)

    D.succeed
        { id = id
        , firstname = firstname
        , lastname = lastname
        }
```

The name being bound sits at the *start* of its line, right next to the
expression that produces it, so a long chain reads top to bottom and it is
hard to get the order of the bindings wrong.

## Desugaring

```elm
\pattern <- source
rest
```

means exactly

```elm
source (\pattern -> rest)
```

The continuation — everything after the line, to the end of the enclosing
expression — becomes a lambda, and that lambda is applied as the **last
argument** of `source`. Consecutive back-lambdas therefore nest to the
right, so this:

```elm
\a <- first
\b <- second a
finish a b
```

is this:

```elm
first (\a -> second a (\b -> finish a b))
```

Nothing else changes: the compiler sees ordinary lambdas and ordinary
function application, with the same types, the same performance, and the
same generated code you would get by writing them out.

Several patterns bind a multi-argument callback (`\a b <- f` is
`f (\a b -> ...)`), and any pattern a normal lambda accepts works, including
destructuring:

```elm
\{ name, age } <- withUser userId
Debug.log name age
```

## Callback-last functions

The continuation is passed as the last argument, so `source` must be a
function that takes its callback last. Most of `elm/core` takes the callback
*first* (`Decode.andThen : (a -> Decoder b) -> Decoder a -> Decoder b`), so
it needs flipping.

For tasks this fork ships it: **`Task.await : Task x a -> (a -> Task x b) -> Task x b`**
is `andThen` with the arguments the other way round.

```elm
\user <- Task.await (fetchUser id)
\posts <- Task.await (fetchPosts user)

Task.succeed (summarize user posts)
```

For anything else, define the flip once:

```elm
await : Decoder a -> (a -> Decoder b) -> Decoder b
await decoder callback =
    D.andThen callback decoder
```

Any name works — `await`, `with`, `do`, `try`. The same shape covers
`Maybe.andThen`, `Result.andThen`, and your own callback APIs.

## Layout rule

The source expression is read with the indentation of the `\`, which means:

- It **ends at the end of the line**, so the next line starts the
  continuation instead of being swallowed as another argument.
- If it needs more lines, **indent them past the `\`**:

  ```elm
  \total <-
      await
          (D.field "total" D.int)
  D.succeed total
  ```

- The continuation lines line up with the `\` itself, which is what the
  examples above do.

A back-lambda cannot be the last thing in a definition (it binds a name for
lines that must exist), and it works anywhere an expression works: `let`
bindings, `case` branches, after `<|`, inside parentheses.

## Errors

```
-- MISSING BODY ---------------------------------------------------- src/E1.elm

This `<-` line has nothing after it:

8|     \id <- D.andThen
       ^
A `<-` line binds a name for the lines that FOLLOW it, so it cannot be the last
thing in a definition. Add the rest of the block underneath, at the same
indentation:

    \user <- Decode.await userDecoder
    Decode.succeed user.name
```

Type errors inside a back-lambda point at the lambda the sugar expands to,
so a mismatch in the continuation reads as a mismatch in an anonymous
function argument.

## Compatibility

`\x <- ...` was a syntax error before, so no existing program changes
meaning. But external tools parse Elm themselves: **elm-format will not
format files using back-lambdas**, and editor grammars will highlight them
oddly, the same as for the other syntax added by this fork. Since this sugar
tends to spread across a codebase rather than sitting in a few files, agree
on a convention before adopting it widely.
