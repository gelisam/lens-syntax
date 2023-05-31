# lens-syntax

This library provides an alternative syntax for composing lenses and other
optics. Here is a comparison between the standard, pointfree syntax and our
pointful syntax based on list comprehensions:

```haskell
-- Some nested data to manipulate using Setters
input :: [Maybe (Int, String)]
input
  = [Just (1, "abc"), Nothing, Just (2, "def")]

-- |
-- >>> pointfreeSyntax
-- [Just (1,"ABC"),Nothing,Just (2,"DEF")]
pointfreeSyntax :: [Maybe (Int, String)]
pointfreeSyntax
  = input & each . _Just . _2 . each %~ toUpper

-- |
-- >>> pointfulSyntax
-- [Just (1,"ABC"),Nothing,Just (2,"DEF")]
pointfulSyntax :: [Maybe (Int, String)]
pointfulSyntax
  = $(Syntax.over [| [ toUpper c
                     | Just (_, s) <- each input
                     , c <- each s
                     ] |])
```

When composing functions, Haskell gives us the choice between using a pointfree
style or a pointful style. Sometimes one style is clearly more readable than
the other, and sometimes both styles are about as readable and so which one to
choose becomes a question of personal preference.

When composing lenses, the `lens` library is only giving us one choice, the
pointfree style. I certainly do not claim that this library's syntax is more
readable in all cases, and in fact at first the pointful syntax is bound to
always look less readable due to unfamiliarity. I am hoping that over time,
we can acquire a taste for when each style is appropriate.

See [more examples here](./test/Main.hs).
