This plugin is ongoing development for my thesis project:

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


---

Annotation documentation at https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#using-annotations broken. I started a discussion in this issue: https://gitlab.haskell.org/ghc/ghc/-/issues/13169#note_410039

PR Merged :)

---

More complicated flip functions do not get optimized:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import HsComprehension

data Palin a where
    Empty :: Palin '[]
    Single :: a -> Palin '[a]
    PCons :: a -> Palin xs -> a -> Palin (a ': xs)

palin = PCons @Int 1 (Single "boe") 2

type family ShowList xs where
    ShowList '[] = '[] ~ '[]
    ShowList (a ': as) = (Show a, ShowList as)

instance ShowList xs => Show (Palin xs) where
    show Empty = ""
    show (Single x) = show x
    show (PCons lhs tail rhs) = show lhs ++ show tail ++ show rhs

flipPalin :: Palin xs -> Palin xs
flipPalin Empty = Empty
flipPalin (Single x) = Single x
flipPalin (PCons lhs tail rhs) = PCons rhs tail lhs
```

then the following happens

```haskell
{-# ANN joe2 CoreTrace #-}
joe2 = flipPalin . flipPalin

-- becomes: 

λxs -> 
    λx -> 
        Case 
            x
         of
            Empty -> $WEmpty
            Single -> $WSingle (@a) x
            PCons -> $WPCons (@a) (@xs) lhs tail rhs

-- i.e. doesn't realize this is identity
```

But alternatively:

```haskell

{-# ANN joe2 CoreTrace #-}
joe2 = flipPalin . flipPalin . flipPalin

-- does becomes: 

flipPalin
```

---

Fusion happens :)

```
joe :: [Int] -> [Int]
joe = map (+1) . map (+1)

--------------------------
Desugared
--------------------------
Found annotated nonrec function named joe
Pretty: 
(map (let v_B1 = + in
    let v_B3 = I# 1# in
        λv_B2 -> 
            v_B1 v_B2 v_B3
        
    
)) . (map (let v_B1 = + in
    let v_B3 = I# 1# in
        λv_B2 -> 
            v_B1 v_B2 v_B3
        
    
))

--------------------------
Simplifier
--------------------------
Found annotated nonrec function named joe
Pretty: 
λx -> 
    build (λb1 -> 
        λc -> 
            λn -> 
                foldr (mapFB c (λx -> 
                    case x of
                        I# x -> I# (2# +# x)
                        
                    
                )) n x
            
        
    )
```


