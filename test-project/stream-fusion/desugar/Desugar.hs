-- Use like so:
--
-- runghc Desugar.hs < Examples.hs > ExamplesDesugared.hs
--
-- and watch out, the pretty printer sometimes misses (..)'s (wtf!!!)

module Main (main) where

import Data.List

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Haskell.Pretty

main = interact $ \input ->
  let ParseOk (HsModule sl m es is decls) = parseModule input
      decls' = [ decl
               | (exp, stmts, context) <- extractListComps decls
               , let exp' = desugar [0..] exp stmts
               , decl <- [context (HsListComp exp stmts), context exp'] ]
   in prettyPrint (HsModule sl m es is decls') ++ "\n"

extractListComps :: [HsDecl] -> [(HsExp, [HsStmt], HsExp -> HsDecl)]
extractListComps decls =
    [ (exp, stmts,
       \rhs -> HsPatBind sl (HsPVar (HsIdent name))
                            (HsUnGuardedRhs rhs) [])
    | HsPatBind sl (HsPVar (HsIdent name))
                   (HsUnGuardedRhs (HsListComp exp stmts)) [] <- decls ]
 ++ [ (exp, stmts,
       \rhs -> HsFunBind [HsMatch sl name ps (HsUnGuardedRhs rhs) ds])
    | HsFunBind [HsMatch sl name ps
                         (HsUnGuardedRhs (HsListComp exp stmts)) ds] <- decls ]

{-

Streams desugaring for list comprehensions:

<< [ e | Q ] >>


<< [ e | ] >> = return e

<< [ e | b, Q ] >> = gaurd b (<< [ e |  Q ] >>)

<< [ e | p <- l, Q ] >> = bind f g l
  where
    f p = Just (x1, ..., xn)
    f _ = Nothing

    g (x1, ..., xn) = << [ e | Q ] >>

<< [ e | let decls, Q ] >> = declare g (let decls in (x1, .., xn))
  where
    g (x1, .., xn) = << [ e | Q ] >>

We can optimise the common cases:


<< [ e | p <- l, b ] >> = mapFilter f l
  where f p | b = Just e
        f _     = Nothing

<< [ e | p <- l, b, Q ] >> = bind f g l
  where f p | b = Just (x1, ..., xn)
        f _     = Nothing
        
        g (x1, ..., xn) = << [ e | Q ] >>


using the auxillary definitions:

List:
-----

return e = [e]

guard True  xs = xs
guard false xs = []

concatMap' f g xs = foldr h [] xs
  where h x ys = case f x of
                  Just bs -> g bs ++ ys
                  Nothing ->         ys

declare g bs = g bs

mapFilter f xs = foldr h [] xs
  where h x ys = case f x of
                  Just y  -> y : ys
                  Nothing ->     ys


Stream:
-------

return e = Stream next True
  where
    next True  = Yield e False
    next False = Done

guard b (Stream next0 s0) = Stream next (b, s0)
  where
    next (False, s) = Done
    next (True, s)  = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    (True, s')
      Yield x s' -> Yield x (True, s')

bind f g (Stream next0 s0) = Stream next (s, Nothing)
  where
    next (s, Nothing) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    (s', Nothing)
      Yield x s' -> case f x of
         Just bs -> Skip (s', Just (g bs))
         Nothing -> Skip (s', Nothing)

    next (s, Just (Stream next1 s1)) = case next1 s1 of
      Done        -> Done
      Skip    s1' -> Skip    (s, Just (Stream next1 s1'))
      Yield x s1' -> Yield x (s, Just (Stream next1 s1'))

declare g bs = Stream next (g bs)
  where next (Stream next0 s) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    (Stream next0 s')
      Yield x s' -> Yield x (Stream next0 s')

mapFilter f (Stream next0 s0) = Stream next s0
  where
    next s = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    s'
      Yield x s' -> case f x of
                  Just y  -> Yield y s'
                  Nothing -> Skip    s'

-}

hsJust       = HsVar (UnQual (HsIdent "Just"))
hsNothing    = HsVar (UnQual (HsIdent "Nothing"))

hsReturn     = HsVar (Qual (Module "L") (HsIdent "return"))
hsGuard      = HsVar (Qual (Module "L") (HsIdent "guard"))
hsConcatMap  = HsVar (Qual (Module "L") (HsIdent "concatMap"))
hsConcatMap' = HsVar (Qual (Module "L") (HsIdent "concatMap'"))
hsDeclare    = HsVar (Qual (Module "L") (HsIdent "declare"))
hsMapFilter  = HsVar (Qual (Module "L") (HsIdent "mapFilter"))
hsMap        = HsVar (Qual (Module "L") (HsIdent "map"))

desugar :: [Int] -> HsExp -> [HsStmt] -> HsExp

-- [ e | ] = return e
desugar ns e [] = HsApp hsReturn e

-- [ e | b, Q ] = gaurd b [ e | Q ]
desugar ns e (HsQualifier b : qs) = HsApp (HsApp hsGuard b)
                                           (HsParen (desugar ns e qs))

-- special case: final generator for simple irrefutable pattern p
-- [ e | p <- l ] = let f p = e
--                   in map f l
desugar (n:ns) e (HsGenerator sloc p@(HsPVar _) l : []) =
  HsLet [HsFunBind [HsMatch sloc (HsIdent (f n)) [p]
                   {- = -} (HsUnGuardedRhs e) []
                   ]
        ]
        (HsApp (HsApp hsMap (HsVar (UnQual (HsIdent (f n)))))
               (HsParen l))

-- special case: final generator
-- [ e | p <- l ] = let f p = Just e
--                      f _ = Nothing
--                   in mapFilter f l
desugar (n:ns) e (HsGenerator sloc p l : []) =
  HsLet [HsFunBind [HsMatch sloc (HsIdent (f n)) [p]
                   {- = -} (HsUnGuardedRhs (HsApp hsJust e)) []
                   ,HsMatch sloc (HsIdent (f n)) [HsPWildCard]
                   {- = -} (HsUnGuardedRhs hsNothing)       []
                   ]
        ]
        (HsApp (HsApp hsMapFilter (HsVar (UnQual (HsIdent (f n)))))
               (HsParen l))

-- special case: final generator with trailing condition
-- [ e | p <- l, b ] = let f p | b = Just e
--                         f _     = Nothing
--                      in mapFilter f l
desugar (n:ns) e (HsGenerator sloc p l : HsQualifier b : []) =
  HsLet [HsFunBind [HsMatch sloc (HsIdent (f n)) [p]
                   {- = -} (HsGuardedRhss [HsGuardedRhs sloc b
                                           (HsApp hsJust e)]) []
                   ,HsMatch sloc (HsIdent (f n)) [HsPWildCard]
                   {- = -} (HsUnGuardedRhs hsNothing) []
                   ]
        ]
        (HsApp (HsApp hsMapFilter (HsVar (UnQual (HsIdent (f n)))))
               (HsParen l))

-- special case: generator with trailing condition
-- [ e | p <- l, b, Q ] = let f p | b = Just (x1, ..., xn)
--                            f _ = Nothing
--                            g (x1, ..., xn) = [ e | Q ]
--                         in concatMap' f g l
desugar (n:ns) e (HsGenerator sloc p l : HsQualifier b : qs) =
  let xs = patBinders p
           `intersect` stmtsFV e qs -- optimisation: only bind used vars
   in HsLet [HsFunBind [HsMatch sloc (HsIdent (f n)) [p]
                       {- = -} (HsGuardedRhss
                                 [HsGuardedRhs sloc b
                                 (HsApp hsJust (mkTupleExpr xs))]) []
                       ,HsMatch sloc (HsIdent (f n)) [HsPWildCard]
                       {- = -} (HsUnGuardedRhs hsNothing) []
                       ]
            ,HsFunBind [HsMatch sloc (HsIdent (g n)) [mkTuplePat xs]
                       {- = -} (HsUnGuardedRhs (desugar ns e qs)) []
                       ]
            ]
            (HsApp (HsApp (HsApp hsConcatMap'
                                 (HsVar (UnQual (HsIdent (f n)))))
                          (HsVar (UnQual (HsIdent (g n)))))
                   (HsParen l))


-- special case: for simple irrefutable pattern p
-- [ e | p <- l, Q ] = let g p = [ e | Q ]
--                      in concatMap g l
desugar (n:ns) e (HsGenerator sloc p@(HsPVar _) l : qs) =
  HsLet [HsFunBind [HsMatch sloc (HsIdent (g n)) [p]
                   {- = -} (HsUnGuardedRhs (desugar ns e qs)) []
                   ]
        ]
        (HsApp (HsApp hsConcatMap (HsVar (UnQual (HsIdent (g n)))))
               (HsParen l))

-- general case of a generator
-- [ e | p <- l, Q ] = let f p = Just (x1, ..., xn)
--                         f _ = Nothing
--                         g (x1, ..., xn) = [ e | Q ]
--                       in concatMap' f g l
desugar (n:ns) e (HsGenerator sloc p l : qs) =
  let xs = patBinders p 
           `intersect` stmtsFV e qs -- optimisation: only bind used vars
   in HsLet [HsFunBind [HsMatch sloc (HsIdent (f n)) [p]
                       {- = -} (HsUnGuardedRhs
                                 (HsApp hsJust (mkTupleExpr xs))) []
                       ,HsMatch sloc (HsIdent (f n)) [HsPWildCard]
                       {- = -} (HsUnGuardedRhs hsNothing) []
                       ]
            ,HsFunBind [HsMatch sloc (HsIdent (g n)) [mkTuplePat xs]
                       {- = -} (HsUnGuardedRhs (desugar ns e qs)) []
                       ]
            ]
            (HsApp (HsApp hsConcatMap (HsVar (UnQual (HsIdent (g n)))))
                   (HsParen l))

{-
-- TODO: need to get binders in decls for this one:

-- [ e | let decls, Q ] = let g (x1, .., xn) = [ e | Q ]
--                         in declare g (let decls in (x1, .., xn))
desugar ns e (HsLetStmt decls : qs) =
  HsLet decls (desugar ns e qs)
-}

patBinders :: HsPat -> [HsName]
patBinders (HsPVar name)         = [name]
patBinders (HsPLit _)            = []
patBinders (HsPNeg p)            = patBinders p
patBinders (HsPInfixApp p1 _ p2) = patBinders p1 ++ patBinders p2
patBinders (HsPApp _ ps)         = concatMap patBinders ps
patBinders (HsPTuple ps)         = concatMap patBinders ps
patBinders (HsPList ps)          = concatMap patBinders ps
patBinders (HsPParen p)          = patBinders p
patBinders (HsPRec _ fields)     = concat [ patBinders p
                                          | HsPFieldPat _ p <- fields ]
patBinders (HsPAsPat _ p)        = patBinders p
patBinders (HsPWildCard)         = []
patBinders (HsPIrrPat p)         = patBinders p


-- The following is for an optimisation, perhaps it's unnecessary.

nameFV (Qual _ _)  = []
nameFV (UnQual n)  = [n]
nameFV (Special _) = []

qopFV (HsQVarOp n) = nameFV n
qopFV _            = []

expFV :: HsExp -> [HsName]
expFV (HsVar n) = nameFV n
expFV (HsCon _) = []
expFV (HsLit _) = []
expFV (HsInfixApp e1 op e2) = qopFV op ++ expFV e1 ++ expFV e2
expFV (HsApp e1 e2) = expFV e1 ++ expFV e2
expFV (HsNegApp e) = expFV e
expFV (HsLambda _ ps e) = nub (expFV e) \\ concatMap patBinders ps
--expFV (HsLet [HsDecl] HsExp)
expFV (HsIf e1 e2 e3) = expFV e1 ++ expFV e2 ++ expFV e3
--expFV (HsCase HsExp [HsAlt])
expFV (HsDo stmts) = stmtsFV unit_con stmts
expFV (HsTuple es) = concatMap expFV es
expFV (HsList es) = concatMap expFV es
expFV (HsParen e) = expFV e
expFV (HsLeftSection e op)  = qopFV op ++ expFV e
expFV (HsRightSection op e) = qopFV op ++ expFV e
--expFV (HsRecConstr HsQName [HsFieldUpdate])	record construction expression
--expFV (HsRecUpdate HsExp [HsFieldUpdate])	record update expression
expFV (HsEnumFrom e) = expFV e
expFV (HsEnumFromTo e1 e2) = expFV e1 ++ expFV e2
expFV (HsEnumFromThen e1 e2) = expFV e1 ++ expFV e2
expFV (HsEnumFromThenTo e1 e2 e3) = expFV e1 ++ expFV e2 ++ expFV e3
expFV (HsListComp e stmts) = stmtsFV e stmts
expFV (HsExpTypeSig _ e _) = expFV e

stmtsFV :: HsExp -> [HsStmt] -> [HsName]
stmtsFV e [] = expFV e
stmtsFV e (HsGenerator _ p e' : stmts) = nub (expFV e' ++ stmtsFV e stmts) \\ patBinders p
stmtsFV e (HsQualifier     e' : stmts) = expFV e' ++ stmtsFV e stmts
--stmtsFV e (HsLetStmt decls) = 

mkTupleExpr []  = HsCon (Special HsUnitCon)
mkTupleExpr [n] = HsVar (UnQual n)
mkTupleExpr ns  = HsTuple [ HsVar (UnQual n) | n <- ns ]

mkTuplePat []  = HsPWildCard
mkTuplePat [n] = HsPVar n
mkTuplePat ns  = HsPTuple [ HsPVar n | n <- ns ]

f n = "f_" ++ show n
g n = "g_" ++ show n












{-

Alternative streams desugaring for list comprehensions:

<< [ e | Q ] P D >> :: Exp -> [Qual] -> (Pat -> Pat) -> Exp -> ([Clause], Exp)

Translation scheme takes the list comp's expression and list of qualifiers
and as extra args, a pattern (with a hole) that gives the state pattern and an
expression giving the state to return to when done. It returns a list of
clauses for a 'next' stepper function and an initialiser expression for the
stream's initial state.

To construct the top level we use:

[ e | Q ] = Stream (FunDef "next" clauses) init
  where (clauses, init) = << [ e | Q ] [.] Done >>


<< [ e | ] P D >> = (next:[], True)
  where
     next P[True]  = Yield e P[False]
     next P[False] = D


<< [ e | b, Q ] P D >> = (next:nexts, Nothing)
  where
     (nexts, init) = << [ e | Q ] P[Just[.]] D >>

     next P[Nothing] | b         = Skip (P[Just init])
                     | otherwise = D


<< [ e | let decls, Q ] P D >> = (next:nexts, Nothing)
  where
     (nexts, init) = << [ e | Q ] P[Just((x_n_0, , x_n_m) :!: [.])] D >>

     next P[Nothing] = Skip P[Just(let decls in (x_n_0, , x_n_m) :!: init)]
     
     {x_n_0, , x_n_m} = DV (decls) -- Defined variables


<< [ e | p <- l, Q ] P >> = (next:nexts, stream l :!: Nothing)
  where
     (nexts, init) = << [ e | Q ] P[(Stream next_n s_n :!: Just (p :!: [.]))]
                                  P[(Stream next_n s_n :!: Nothing)] >>

     next P[(Stream next_n s_n :!: Nothing)] = case next_n s_n of
       Done         -> D
       Skip    s_n' -> Skip P[(Stream next_n s_n' :!: Nothing)]
       Yield x s_n' -> case x of
                  p -> Skip P[(Stream next_n s_n' :!: Just (x :!: init))]
                  _ -> Skip P[(Stream next_n s_n' :!: Nothing)]

-}
