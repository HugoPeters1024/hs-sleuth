{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module CoreCollection where

import Data.Data

import GHC
import GHC.Plugins

data CoreTrace = CoreTrace deriving (Show, Data)

data PassInfo = PassInfo { idx :: Int
                         , title :: String
                         , ast :: String
                         , raw :: [CoreBind]
                         }

-- Basically maps all Rec binding groups to seperate NonRecs
getAllTopLevelDefs :: [CoreBind] -> [CoreBind]
getAllTopLevelDefs binds = concatMap go binds
    where go (NonRec b e) = [NonRec b e]
          go (Rec bs) = map (uncurry NonRec) bs

annotationsOn :: forall a. Data a => ModGuts -> CoreBndr -> CoreM [a]
annotationsOn guts bndr = do
  (_, anns) <- getAnnotations (deserializeWithData @a) guts
  return $ lookupWithDefaultUFM_Directly anns [] (varUnique bndr)

isCoreTraced :: ModGuts -> CoreBndr -> CoreM Bool
isCoreTraced guts b = (not . null) <$> annotationsOn @CoreTrace guts b

annotatedFunctions :: ModGuts -> [(CoreBndr, CoreExpr)] -> CoreM [String]
annotatedFunctions guts = mapMaybeM f
    where f :: (CoreBndr, CoreExpr) -> CoreM (Maybe String)
          f (b, _) = do
              dflags <- getDynFlags
              anns <- annotationsOn @CoreTrace guts b
              pure $ if length anns > 0 
                        then Just (showSDoc dflags (ppr b))
                        else Nothing

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f [] =  pure []
mapMaybeM f (x:xs) = f x >>= \case
    Just x -> (x:) <$> mapMaybeM f xs
    Nothing -> mapMaybeM f xs
