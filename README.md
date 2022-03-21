This plugin is ongoing development for my thesis project:

### Extra deps

- Syntax highlighting is done with a python script that uses pygments, make sure it is installed globally

###  Useful links

- Calling GHC from haskell: https://stackoverflow.com/questions/35449353/how-can-i-parse-ghc-core
- Haskell to core: https://serokell.io/blog/haskell-to-core

- https://arxiv.org/pdf/1803.07130.pdf (Inspection testing)
- https://www.cs.tufts.edu/~nr/cs257/archive/duncan-coutts/stream-fusion.pdf

- ghc-core exposes a map with CoreExprs as keys: https://ghc.gitlab.haskell.org/ghc/doc/libraries/ghc-9.3/GHC-Core-Map-Expr.html

- high level overview of GHC (with Simon as an author): https://www.aosabook.org/en/ghc.html

### What is the plan?

- An output that describes precisely what optimisation are done.
- This can currently already be done, but the output is very cluttered and not intuitive.
- Ideally the transformations applied at each pass can be derived and specified in such a way that
  they can be reproduced on the original source code.
- This requires a well defined transformation for each change.
- To confirm wether this possible a very simple toy language would be in order, allowing for a simple way to expriment
how to be resilient to different kinds of noise introduced by artifacts

### Useful findings

- The build/foldr artifacts that appear in the core expressions are the result of the deforestation algorithm
  (i.e. fusion to prevent intermediate datastructures)

- This plugin is related https://github.com/bgamari/ghc-dump but is not up to date and not really easy to use,
  the pretty printer looks good though



### Scratch Board

diff command:

```<pre>diff --color=always -s src/HsComprehension.hs src/HsComprehension2.hs | ansi2html --partial &gt; output.html
</pre>```


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


---

What is this?? why unpack each cons application as a global binding?

```haskell
{-# RULES
  "mp/mp"    joe = joe2
    #-}



{-# ANN joe CoreTrace #-}
{-# NOINLINE joe #-}
joe :: [Int]
joe =  map (+1) [1,2,3,4,5]

joe2 :: [Int]
joe2 = error "U got bamboozled"
```


```
Found annotated nonrec function named joe2
Pretty: 
error (lvl_s7Ot) (unpackCString# lvl_s7Ou)

Found annotated nonrec function named joe_s7O7
Pretty: 
I# 2#

Found annotated nonrec function named joe_s7O8
Pretty: 
I# 3#

Found annotated nonrec function named joe_s7O9
Pretty: 
I# 4#

Found annotated nonrec function named joe_s7Oa
Pretty: 
I# 5#

Found annotated nonrec function named joe_s7Ob
Pretty: 
I# 6#

Found annotated nonrec function named joe_s7Oc
Pretty: 
joe_s7Ob : []

Found annotated nonrec function named joe_s7Od
Pretty: 
joe_s7Oa : joe_s7Oc

Found annotated nonrec function named joe_s7Oe
Pretty: 
joe_s7O9 : joe_s7Od

Found annotated nonrec function named joe_s7Of
Pretty: 
joe_s7O8 : joe_s7Oe

Found annotated nonrec function named joe
Pretty: 
joe_s7O7 : joe_s7Of
```

---

This step introduces a variable `ww_s7DD` in the function oneMore which seems to reference
a variable from the auxilary $woneMore function that is not in scope (see the list of global bindings as well).
How is this a valid core expression?

```
--------------------------
Worker Wrapper binds
--------------------------
attempting to locate: ["oneMore"]
main_s747
main
main_s7Df
:Main.main
$trModule_s7Dg
$trModule_s7Dh
$trModule_s7Di
$trModule_s7Dj
Main.$trModule
lvl_s7Dv
$woneMore
oneMore
Found annotated function named $woneMore
Pretty: 
λww -> 
    let w_s7Dw = I# ww in
        case let ds_d6W7 = w_s7Dw in
            case ds_d6W7 of
                I# ds_d6W8 -> case ds_d6W8 of
                    0# -> lvl_s7Dv
                    _ -> oneMore (I# (ds_X1F -# 2#))
                    
                
                
            
         of
            I# ww_s7DD -> ww_s7DD
            
        
    


Found annotated function named oneMore
Pretty: 
λw_s7Dw -> 
    case case w_s7Dw of
        I# ww -> $woneMore ww
        
     of
        _ -> I# ww_s7DD
        
```

---

`-ddump-core2core` gives some really extensive inlining information:

```
Considering inlining: $fNumInt_$c+
  arg infos [NonTrivArg, NonTrivArg]
  interesting continuation BoringCtxt
  some_benefit True
  is exp: True
  is work-free: True
  guidance ALWAYS_IF(arity=2,unsat_ok=True,boring_ok=False)
  ANSWER = YES
Inlining done: GHC.Num.$fNumInt_$c+
    Inlined fn:  \ (ds_a70R [Occ=Once1!] :: GHC.Types.Int)
                   (ds1_a70S [Occ=Once1!] :: GHC.Types.Int) ->
                   case ds_a70R of { GHC.Types.I# x_a70U [Occ=Once1] ->
                   case ds1_a70S of { GHC.Types.I# y_a70X [Occ=Once1] ->
                   GHC.Types.I# (GHC.Prim.+# x_a70U y_a70X)
                   }
                   }
    Cont:   ApplyToVal nodup hole GHC.Types.Int
                                  -> GHC.Types.Int -> GHC.Types.Int
              (Main.oneMore (GHC.Types.I# 5#))
            ApplyToVal nodup hole GHC.Types.Int -> GHC.Types.Int
              (Main.oneMore (GHC.Types.I# 6#))
            Select nodup wild_a78t
            Stop[BoringCtxt] GHC.Base.String
Considering inlining: oneMore
  arg infos [ValueArg]
  interesting continuation BoringCtxt
  some_benefit True
  is exp: True
  is work-free: True
  guidance IF_ARGS [20] 51 20
  case depth = 0
  depth based penalty = 0
  discounted size = 11
  ANSWER = YES
Inlining done: Main.oneMore
    Inlined fn:  \ (ds_d70y [Occ=Once1!] :: GHC.Types.Int) ->
                   case ds_d70y of { GHC.Types.I# ds_d70z [Occ=Once1!] ->
                   case ds_d70z of ds_X1 [Occ=Once1] {
                     __DEFAULT -> GHC.Types.I# (GHC.Prim.+# ds_X1 1#);
                     0# -> GHC.Types.I# 1#
                   }
                   }
    Cont:   ApplyToVal nodup hole GHC.Types.Int -> GHC.Types.Int
              (GHC.Types.I# 5#)
            Select nodup wild_a70T
            Select nodup wild_a78t
            Stop[BoringCtxt] GHC.Base.String
Considering inlining: oneMore
  arg infos [ValueArg]
  interesting continuation BoringCtxt
  some_benefit True
  is exp: True
  is work-free: True
  guidance IF_ARGS [20] 51 20
  case depth = 0
  depth based penalty = 0
  discounted size = 11
  ANSWER = YES
Inlining done: Main.oneMore
    Inlined fn:  \ (ds_d70y [Occ=Once1!] :: GHC.Types.Int) ->
                   case ds_d70y of { GHC.Types.I# ds_d70z [Occ=Once1!] ->
                   case ds_d70z of ds_X1 [Occ=Once1] {
                     __DEFAULT -> GHC.Types.I# (GHC.Prim.+# ds_X1 1#);
                     0# -> GHC.Types.I# 1#
                   }
                   }
    Cont:   ApplyToVal nodup hole GHC.Types.Int -> GHC.Types.Int
              (GHC.Types.I# 6#)
            Select nodup wild1_a70W
            Select nodup wild_a78t
            Stop[BoringCtxt] GHC.Base.String
```



