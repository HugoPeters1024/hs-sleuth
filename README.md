This plugin is ongoing development for my thesis project:

## Developement Scratchboard

###  Useful links

- Calling GHC from haskell: https://stackoverflow.com/questions/35449353/how-can-i-parse-ghc-core
- Haskell to core: https://serokell.io/blog/haskell-to-core

### Scratch Board

I don't understand the different outputs from the compiler flag and this plugin. Check out this example:

```
oneMore :: Int -> Int
oneMore x = let y = x +1 in y * y
```

`-ddump-ds-preopt` gives.

```
-- RHS size: {terms: 17, types: 9, coercions: 0, joins: 0/4}
oneMore :: Int -> Int
[LclIdX]
oneMore
  = \ (x_ayB :: Int) ->
      letrec {
        y_ayC :: Int
        [LclId]
        y_ayC
          = let {
              $dNum_aL9 :: Num Int
              [LclId]
              $dNum_aL9 = GHC.Num.$fNumInt } in
            let {
              $dNum_aLU :: Num Int
              [LclId]
              $dNum_aLU = $dNum_aL9 } in
            letrec {
              y_aLV :: Int
              [LclId]
              y_aLV = + @Int $dNum_aL9 x_ayB (ghc-prim:GHC.Types.I# 1#); } in
            y_aLV; } in
      * @Int $dNum_aM0 y_ayC y_ayC

```

Why the chain of assignments of fNumInt and the letrec for y??

But my plugin shows (without type applications and typeclasses for readability):

```
Found a nonrec function named oneMore
Pretty: 
λx -> 
    let y = + x (I# 1#)
         in * y y
    
```

Note the absence of the extra lets. The current oneMore is actually also not one more because without two use sites of y the let
already gets inlined before my plugin is called with seemingly no option to disable these apparent invisible simplifier steps.
