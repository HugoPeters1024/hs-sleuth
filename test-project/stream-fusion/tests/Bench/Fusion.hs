{-# OPTIONS -cpp #-}
--
-- Test the results of fusion
--

--
-- N.B. make sure to disable down fusion when using only loopU fusion
--

import Data.Char
import Bench.Utils
import Text.Printf
import qualified Data.List.Stream as P -- ours

-- minimum pipelines to trigger the various fusion forms
tests =
 [("force0",          [F  (P.maximum)])
 ,("force1",          [F  (P.map (+1))])

-- non directional
 ,("map/map",
    [F  (
        P.map (*2) . P.map (+4)                             -- 1
    )])
 ,("filter/filter",
    [F  (
        P.filter (/=101) . P.filter (/=102)                 -- 1
    )])
 ,("filter/map",
    [F  (
        P.filter (/=103) . P.map (+5)                       -- 1
    )])
 ,("map/filter",
    [F  (
        P.map (*3) . P.filter (/=104)                       -- 1
    )])
 ,("map/noacc",
    [F  (
        (P.map (+1) . P.filter (/=112)) . P.map (*2)         -- 2
    )])
 ,("noacc/map",
    [F  (
        P.map (+1) . (P.map (+2) . P.filter (/=113))         -- 2
    )])
 ,("filter/noacc",
    [F  (
        (P.map (+1) . P.filter (/=101)) . P.filter (/=114)   -- 2
    )])
 ,("noacc/filter",
    [F  (
        P.filter (/=101) . (P.map (*2) . P.filter (/=115))  -- 2
    )])
 ,("noacc/noacc",
    [F  (
        (P.map (*3) . P.filter (/=108)) . (P.map (*4) . P.filter (/=109)) -- 3
    )])

-- up loops
 ,("up/up",
    [F  (
        P.foldl' (const.(+1)) (0::X) . P.scanl (flip const) (0::W) -- 1
    )])
 ,("map/up",
    [F  (
        P.foldl' (const.(+6)) (0::X) . P.map (*4) -- 1
    )])
 ,("up/map",
    [F  (
        P.map (+7) . P.scanl const (0::W) -- 1
    )])
 ,("filter/up",
    [F  (
        P.foldl' (const.(+8)) (0::X) . P.filter (/=105) -- 1
    )])
 ,("up/filter",
    [F  (
        P.filter (/=106) . P.scanl (flip const) (0::W) -- 1
    )])
 ,("noacc/up",
    [F  (
        P.foldl' (const.(+1)) (0::W) . (P.map (+1) . P.filter (/=110)) -- 2
    )])
 ,("up/noacc",
    [F  (
        (P.map (+1) . P.filter (/=111)) . P.scanl (flip const) (0::W) -- 2
    )])

-- down loops
 ,("down/down",
    [F  (
        P.foldr (const (+9))  (0::W) . P.scanl (flip const) (0::W) -- 1
    )])
 ,("map/down",
    [F  (
        P.foldr (const (+10)) (0::W) . P.map (*2) -- 1
    )]) 
 ,("down/map",
    [F  (
        P.map (*2) . P.scanl (flip const) (0::W) -- 1
    )])
 ,("filter/down",
    [F  (
        P.foldr (const (+11)) (0::W) . P.filter (/=106) -- 1
    )])
 ,("down/filter",
    [F  (
        P.filter (/=107) . P.scanl (flip const) (0::W) -- 1
    )])
 ,("noacc/down",
    [F  (
        P.foldr (const (+1)) (0::W) . (P.map (+1) . P.filter (/=116)) -- 2
    )])
 ,("down/noacc",
    [F  (
        (P.map (+1) . P.filter (/=101)) . P.scanl (flip const) (0::W) -- 2
    )])

-- misc
 ,("length/loop",
    [F  (
        P.length  . P.filter (/=105) -- 1
    )])
 ,("maximum/loop",
    [F  (
        P.maximum . P.map (*4) -- 1
    )])
 ,("minimum/loop",
    [F  (
        P.minimum . P.map (+6) -- 1
    )])

 ]

-- and some longer ones to see the full effect
bigtests =
 [("big map/map",
    [F  (P.map (subtract 3). P.map (+7) . P.map (*2) . P.map (+4) -- 3
    )])
 ,("big filter/filter",
    [F  (P.filter (/=103) . P.filter (/=104) . P.filter (/=101) .  P.filter (/=102) -- 3
    )])
 ,("big filter/map",
    [F  (P.map (*2) . P.filter (/=104) . P.map (+6) . P.filter (/=103) .  P.map (+5) --4 
    )])
 ]

main = do
    force (string,string)
    printf "#Byte\n"
    run 5 (map (fromIntegral.ord) string) (tests ++ bigtests)

