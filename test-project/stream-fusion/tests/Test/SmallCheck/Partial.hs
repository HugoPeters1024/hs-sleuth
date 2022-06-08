{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NPlusKPatterns       #-}


---------------------------------------------------------------------
-- SmallCheck: another lightweight testing library.
-- Colin Runciman, August 2006
-- Version 0.2 (November 2006)
--
-- After QuickCheck, by Koen Claessen and John Hughes (2000-2004).
---------------------------------------------------------------------

module Test.SmallCheck.Partial (
  smallCheck, smallCheckI, depthCheck, test,
  Property, Testable,
  forAll, forAllElem,
  exists, existsDeeperBy, thereExists, thereExistsElem,
  (==>),
  Series, Serial(..),
  (\/), (><), two, three, four,
  cons0, cons1, cons2, cons3, cons4,
  alts0, alts1, alts2, alts3, alts4,
  PositiveIntegral(..), Nat, Natural,
  depth, inc, dec,
  
  run, runTests, TestOptions(..)
  ) where

import Data.Data

import Data.List (intersperse)
import Control.Monad (when)
import System.IO (stdout, hFlush)
import System.IO.Unsafe (unsafePerformIO)  -- used only for Testable (IO a)

import qualified Test.ChasingBottoms as Bottoms

------------------ <Series of depth-bounded values> -----------------

-- Series arguments should be interpreted as a depth bound (>=0)
-- Series results should have finite length

type Series a = Int -> [a]

-- sum
infixr 7 \/
(\/) :: Series a -> Series a -> Series a
(s1 \/ s2) 0 = bottom : []
(s1 \/ s2) d = bottom : s1 (d-1) ++ s2 (d-1)

-- product
infixr 8 ><
(><) :: Series a -> Series b -> Series (a,b)
(s1 >< s2) 0 = bottom : []
(s1 >< s2) d = bottom : [(x,y) | x <- s1 (d-1), y <- s2 (d-1)]


------------------- <methods for type enumeration> ------------------

-- enumerated data values should be finite and fully defined
-- enumerated functional values should be total and strict

bottom :: a
bottom = error "_|_"

-- bounds:
-- for data values, the depth of nested constructor applications
-- for functional values, both the depth of nested case analysis
-- and the depth of results
 
class Serial a where
  series   :: Series a
  coseries :: Serial b => Series (a->b)

instance Serial () where
  series   d = take (d+1) [bottom, ()]
  coseries d = [ \() -> b
               | b <- series d ]

instance Serial Int where
  series   d = seq 0
    where seq n | n > d = []
          seq n@0       = bottom         : seq (n+1)
          seq n@1       = 0              : seq (n+1)
          seq n         = -(n-1) : (n-1) : seq (n+1)
  coseries d = [ \i -> if i > 0 then f (N (i - 1))
                       else if i < 0 then g (N (abs i - 1))
                       else z
               | z <- alts0 d, f <- alts1 d, g <- alts1 d ]

instance Serial Integer where
  series   d = [ toInteger (i :: Int)
               | i <- series d ]
  coseries d = [ f . (fromInteger :: Integer->Int)
               | f <- series d ]

newtype PositiveIntegral a = N a

instance Show a => Show (PositiveIntegral a) where
  show (N i) = show i

instance (Integral a, Serial a) => Serial (PositiveIntegral a) where
  series   d = map N [0..d']
               where
               d' = fromInteger (toInteger d)
  coseries d = [ \(N i) -> if i > 0 then f (N (i - 1))
                           else z
               | z <- alts0 d, f <- alts1 d ]

type Nat = PositiveIntegral Int
type Natural = PositiveIntegral Integer

instance Serial Float where
  series d   = [ encodeFloat sig exp
               | (sig,exp) <- series d,
                 odd sig || sig==0 && exp==0 ]
  coseries d = [ f . decodeFloat
               | f <- series d ]
             
instance Serial Double where
  series   d = [ frac (x :: Float)
               | x <- series d ]
  coseries d = [ f . (frac :: Double->Float)
               | f <- series d ]

frac :: (Real a, Fractional a, Real b, Fractional b) => a -> b
frac = fromRational . toRational

instance Serial Char where
  series d   = take (d+1) " \nab\0cd,ef"
  coseries d = [ \c -> f (N (fromEnum c - fromEnum 'a'))
               | f <- series d ]

instance (Serial a, Serial b) =>
         Serial (a,b) where
  series   = series >< series
  coseries = map uncurry . coseries

instance (Serial a, Serial b, Serial c) =>
         Serial (a,b,c) where
  series   = \d -> bottom
                 : [ (x,y,z)
                   | x <- series (d-1)
                   , y <- series (d-1)
                   , z <- series (d-1)]
  coseries = map uncurry3 . coseries

instance (Serial a, Serial b, Serial c, Serial d) =>
         Serial (a,b,c,d) where
  series   = \d -> bottom
                 : [ (x,y,z,a)
                   | x <- series (d-1)
                   , y <- series (d-1)
                   , z <- series (d-1)
                   , a <- series (d-1)]
  coseries = map uncurry4 . coseries

uncurry3 :: (a->b->c->d) -> ((a,b,c)->d)
uncurry3 f (x,y,z) = f x y z

uncurry4 :: (a->b->c->d->e) -> ((a,b,c,d)->e)
uncurry4 f (w,x,y,z) = f w x y z

two   :: Series a -> Series (a,a)
two   s = s >< s

three :: Series a -> Series (a,a,a)
three s = \d -> [(x,y,z) | (x,(y,z)) <- (s >< s >< s) d]

four  :: Series a -> Series (a,a,a,a)
four  s = \d -> [(w,x,y,z) | (w,(x,(y,z))) <- (s >< s >< s >< s) d]

cons0 :: 
         a -> Series a
cons0 c _ = [c]

cons1 :: Serial a =>
         (a->b) -> Series b
cons1 c d = [c z | d > 0, z <- series (d-1)]

cons2 :: (Serial a, Serial b) =>
         (a->b->c) -> Series c
cons2 c d = [c y z | d > 0, y <- series (d-1), z <- series (d-1)]

cons3 :: (Serial a, Serial b, Serial c) =>
         (a->b->c->d) -> Series d
cons3 c d = [c x y z | d > 0, (x,y,z) <- series (d-1)]

cons4 :: (Serial a, Serial b, Serial c, Serial d) =>
         (a->b->c->d->e) -> Series e
cons4 c d = [c w x y z | d > 0, (w,x,y,z) <- series (d-1)]

alts0 ::  Serial a =>
            Series a
alts0 d = series d

alts1 ::  (Serial a, Serial b) =>
            Series (a->b)
alts1 d = if d > 0 then series (dec d)
          else [\_ -> x | x <- series d]

alts2 ::  (Serial a, Serial b, Serial c) =>
            Series (a->b->c)
alts2 d = if d > 0 then series (dec d)
          else [\_ _ -> x | x <- series d]

alts3 ::  (Serial a, Serial b, Serial c, Serial d) =>
            Series (a->b->c->d)
alts3 d = if d > 0 then series (dec d)
          else [\_ _ _ -> x | x <- series d]

alts4 ::  (Serial a, Serial b, Serial c, Serial d, Serial e) =>
            Series (a->b->c->d->e)
alts4 d = if d > 0 then series (dec d)
          else [\_ _ _ _ -> x | x <- series d]

instance Serial Bool where
  series     = cons0 False \/ cons0 True
  coseries d = [ \x -> if x then b1 else b2
               | b1 <- series d, b2 <- series d ]

instance Serial Ordering where
  series     = cons0 EQ \/ cons0 LT \/ cons0 GT
  coseries d = [ \m -> case m of
                       LT -> x
                       EQ -> y
                       GT -> z
               |  x <- alts0 d ,
                  y <- alts0 d ,
                  z <- alts0 d ]

instance Serial a => Serial (Maybe a) where
  series     = cons0 Nothing \/ cons1 Just
  coseries d = [ \m -> case m of
                       Nothing -> z
                       Just x  -> f x
               |  z <- alts0 d ,
                  f <- alts1 d ]

instance (Serial a, Serial b) => Serial (Either a b) where
  series     = cons1 Left \/ cons1 Right
  coseries d = [ \e -> case e of
                       Left x  -> f x
                       Right y -> g y
               |  f <- alts1 d ,
                  g <- alts1 d ]

instance Serial a => Serial [a] where
  series     = cons0 [] \/ cons2 (:)
  coseries d = [ \xs -> case xs of
                        []      -> y
                        (x:xs') -> f x xs'
               |   y <- alts0 d ,
                   f <- alts2 d ]

-- Warning: the coseries instance here may generate duplicates.
instance (Serial a, Serial b) => Serial (a->b) where
  series 0     = bottom : []
  series (d+1) = bottom : [ \_ -> x | x <- series d ] ++ tail (coseries d)
  coseries d = [ \f -> g [f x | x <- series d]
               | g <- series d ]

-- For customising the depth measure.  Use with care!

depth :: Int -> Int -> Int
depth d d' | d >= 0    = d'+1-d
           | otherwise = error "SmallCheck.depth: argument < 0"

dec :: Int -> Int
dec d | d > 0     = d-1
      | otherwise = error "SmallCheck.dec: argument <= 0"

inc :: Int -> Int
inc d = d+1
{-
-- show the extension of a function (in part, bounded both by
-- the number and depth of arguments)
instance (Serial a, Show a, Show b) => Show (a->b) where
  show f = 
    if maxarheight == 1
    && sumarwidth + length ars * length "->;" < widthLimit then
      "{"++(
      concat $ intersperse ";" $ [a++"->"++r | (a,r) <- ars]
      )++"}"
    else
      concat $ [a++"->\n"++indent r | (a,r) <- ars]
    where
    ars = take lengthLimit [ (show x, show (f x))
                           | x <- series depthLimit ]
    maxarheight = maximum  [ max (height a) (height r)
                           | (a,r) <- ars ]
    sumarwidth = sum       [ length a + length r 
                           | (a,r) <- ars]
    indent = unlines . map ("  "++) . lines
    height = length . lines
    (widthLimit,lengthLimit,depthLimit) = (80,20,3)::(Int,Int,Int)
-}
instance (Data a, Data b, Serial a, Bottoms.ApproxShow a, Bottoms.ApproxShow b) => Show (a->b) where
  show f
    | Bottoms.isBottom f = "_|_"
    | otherwise =
    if maxarheight == 1
    && sumarwidth + length ars * length "->;" < widthLimit then
      "{"++(
      concat $ intersperse ";" $ [a++"->"++r | (a,r) <- ars]
      )++"}"
    else
      concat $ [a++"->\n"++indent r | (a,r) <- ars]
    where
    ars = take lengthLimit [ (Bottoms.approxShow 1000 x, Bottoms.approxShow 1000 (f x))
                           | x <- series depthLimit ]
    maxarheight = maximum  [ max (height a) (height r)
                           | (a,r) <- ars ]
    sumarwidth = sum       [ length a + length r 
                           | (a,r) <- ars]
    indent = unlines . map ("  "++) . lines
    height = length . lines
    (widthLimit,lengthLimit,depthLimit) = (80,20,3)::(Int,Int,Int)


---------------- <properties and their evaluation> ------------------

-- adapted from QuickCheck originals: here results come in lists,
-- properties have depth arguments, stamps (for classifying random
-- tests) are omitted, existentials are introduced

newtype PR = Prop [Result]

data Result = Result {ok :: Maybe Bool, arguments :: [String]}

nothing :: Result
nothing = Result {ok = Nothing, arguments = []}

result :: Result -> PR
result res = Prop [res]

newtype Property = Property (Int -> PR)

class Testable a where
  property :: a -> Int -> PR

instance Testable Bool where
  property b _ = Prop [Result (Just b) []]

instance Testable PR where
  property prop _ = prop

instance (Data a, Serial a, Bottoms.ApproxShow a, Testable b) => Testable (a->b) where
  property f = f' where Property f' = forAll series f

instance Testable Property where
  property (Property f) d = f d

-- For testing properties involving IO.  Unsafe, so use with care!
instance Testable a => Testable (IO a) where
  property = property . unsafePerformIO

evaluate :: Testable a => a -> Series Result
evaluate x d = rs where Prop rs = property x d

forAll :: (Bottoms.ApproxShow a, Testable b) => Series a -> (a->b) -> Property
forAll xs f = Property $ \d -> Prop $
  [ r{arguments = Bottoms.approxShow 1000 x : arguments r}
  | x <- xs d, r <- evaluate (f x) d ]

forAllElem :: (Bottoms.ApproxShow a, Testable b) => [a] -> (a->b) -> Property
forAllElem xs = forAll (const xs)

thereExists :: Testable b => Series a -> (a->b) -> Property
thereExists xs f = Property $ \d -> Prop $
  [ Result
      ( Just $ or [ all pass (evaluate (f x) d)
                  | x <- xs d ] )
      [] ] 
  where
  pass (Result Nothing _)  = True
  pass (Result (Just b) _) = b

thereExistsElem :: Testable b => [a] -> (a->b) -> Property
thereExistsElem xs = thereExists (const xs)

exists :: (Serial a, Testable b) =>
            (a->b) -> Property
exists = thereExists series

existsDeeperBy :: (Serial a, Testable b) =>
                    (Int->Int) -> (a->b) -> Property
existsDeeperBy f = thereExists (series . f)
 
infixr 0 ==>

(==>) :: Testable a => Bool -> a -> Property
True ==>  x = Property (property x)
False ==> x = Property (const (result nothing))

--------------------- <top-level test drivers> ----------------------

-- similar in spirit to QuickCheck but with iterative deepening

test :: Testable a => a -> IO ()
test = smallCheckI

-- test for values of depths 0..d stopping when a property
-- fails or when it has been checked for all these values
smallCheck :: Testable a => Int -> a -> IO ()
smallCheck d = iterCheck 0 (Just d)

-- interactive variant, asking the user whether testing should
-- continue/go deeper after a failure/completed iteration
smallCheckI :: Testable a => a -> IO ()
smallCheckI = iterCheck 0 Nothing

depthCheck :: Testable a => Int -> a -> IO ()
depthCheck d = iterCheck d (Just d)

iterCheck :: Testable a => Int -> Maybe Int -> a -> IO ()
iterCheck dFrom mdTo t = iter dFrom
  where
  iter d = do
    putStrLn ("Depth "++show d++":")
    let Prop results = property t d
    ok <- check (mdTo==Nothing) 0 0 True results
    maybe (whenUserWishes "  Deeper" () $ iter (d+1))
          (\dTo -> when (ok && d < dTo) $ iter (d+1))
          mdTo

check :: Bool -> Int -> Int -> Bool -> [Result] -> IO Bool
check i n x ok rs | null rs = do
  putStr ("  Completed "++show n++" test(s)")
  putStrLn (if ok then " without failure." else ".")
  when (x > 0) $
    putStrLn ("  But "++show x++" did not meet ==> condition.")
  return ok
check i n x ok (Result Nothing _ : rs) = do
  progressReport i n x
  check i (n+1) (x+1) ok rs
check i n x f (Result (Just True) _ : rs) = do
  progressReport i n x
  check i (n+1) x f rs
check i n x f (Result (Just False) args : rs) = do
  putStrLn ("  Failed test no. "++show (n+1)++". Test values follow.")
  mapM_ (putStrLn . ("  "++)) args
  ( if i then
      whenUserWishes "  Continue" False $ check i (n+1) x False rs
    else
      return False )

whenUserWishes :: String -> a -> IO a -> IO a
whenUserWishes wish x action = do
  putStr (wish++"? ")
  hFlush stdout
  reply <- getLine
  ( if (null reply || reply=="y") then action
    else return x )

progressReport :: Bool -> Int -> Int -> IO ()
progressReport i n x | n >= x = do
  when i $ ( putStr (n' ++ replicate (length n') '\b') >>
             hFlush stdout )
  where
  n' = show n


run :: Testable a => a -> Property
run p = Property (property p)

data TestResult = TestOk        Int
                | TestExhausted Int
                | TestFailed    Int [String]

data TestOptions = TestOptions {
                     testDepth :: Int,
                     maxTests  :: Int
                   }

runTest :: TestOptions -> Property -> TestResult
runTest TestOptions { testDepth = depth, maxTests = max } p =
  let Prop results = property p depth
   in check 0 results
  
  where check n _ | n >= max                    = TestExhausted n
        check n                             []  = TestOk n
        check n (Result Nothing      _    : rs) = check (n+1) rs
        check n (Result (Just True ) _    : rs) = check (n+1) rs
        check n (Result (Just False) args : rs) = TestFailed n args
  

runTests :: String -> TestOptions -> [Property] -> IO ()
runTests name opts props =

  do putStr (rjustify 25 name ++ " : ")
     failures <- tr 1 props [] 0
     mapM fa (reverse failures)
     return ()
  where
	rjustify n s = replicate (max 0 (n - length s)) ' ' ++ s

	tr n [] xs c = do
			putStr (rjustify (max 0 (35-n)) " (" ++ show c ++ ")\n")
			return xs
	tr n (prop:props) failures c = 
	   case runTest opts prop of
		TestOk m
			-> do { putStr "." ;
			       tr (n+1) props failures (c+m) }
		TestExhausted m
			-> do { putStr "?" ;
			       tr (n+1) props failures (c+m) }
	  	TestFailed m args
			-> do { putStr "#" ;
			        tr (n+1) props ((args,n,m):failures) c }

	fa :: ([String],Int,Int) -> IO ()
	fa (f,n,m) = 
	  do putStr "\n"
	     putStr ("    ** test " 
			++ show (n  :: Int)
			++ " of "
			++ name
			++ " failed (after " ++ show m ++ " steps) with the binding(s)\n")
	     sequence_ [putStr ("    **   " ++ v ++ "\n")
			| v <- f ]
  	     putStr "\n"


------------------------------------------------------------------------
--
-- foldl'
-- lines too strict
-- structure of domain, insert bottoms in correct position.
--
