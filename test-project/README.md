## Background:

### Simplifier passes

Unintuitively, this is the ordered list of simplifier phases: `[gentle, 2, 1, 0]`


## Test 1: Stream Fusion Rewrite Rule Failing

given the function

```haskell
addThree :: [Int] -> [Int]
addThree = map (+1) . map(+2)
```

The following rewrite rules fire in the first simplifier pass (and none later):

```haskell
{-
    RULES FIRED:
    Class op + (BUILTIN)
    Class op + (BUILTIN)
    map -> fusible (Data.List.Stream)
    map -> fusible (Data.List.Stream)
    STREAM stream/unstream fusion (Data.Stream)
    STREAM map/map fusion (Data.Stream)
    +# (BUILTIN)
-}
```

This yields a nicely fused result:

```haskell
addThree :: [Int] -> [Int]
addThree x = unstream (map lvl (stream x))
```

However, the stream/unstream is still there, making this likely
not a cheaper expression than an unfused canonical map composition.
However, this rule exists in `Data.List.Stream`

```haskell
{-# RULES
"map -> fusible" [~1] forall f xs.
    map f xs = unstream (Stream.map f (stream xs))
--"map -> unfused" [1] forall f xs.
--    unstream (Stream.map f (stream xs)) = map f xs
  #-}
```

Here we see two cyclical rules. One enables the stream fusion, and the other removes it. Using the phase control they should
complement each other. However, in phase 1 the second rule is not being called. And it is not clear why.
