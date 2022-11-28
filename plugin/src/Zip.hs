module Main where

import Prelude as P
import Control.Monad
import Data.Maybe
import System.Environment
import System.FilePath.Posix as FP
import System.Directory as FP
import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as BSL

import HsComprehension.Plugin

main :: IO ()
main = do
  slug <- parseCmdLineOptions <$> getArgs
  let cv = defaultCaptureView

  let dump_dir = coreDumpDir cv slug
  putStrLn $ "Attempting to archive dump files in " ++ dump_dir

  isDir <- FP.doesDirectoryExist dump_dir
  when (not isDir) $ FP.makeAbsolute dump_dir >>= \path -> error (path ++ " does not exist")

  allFileNames <- FP.getDirectoryContents (coreDumpDir cv slug)
  entries <- forM allFileNames $ \fname -> do
    let path = coreDumpDir cv slug `FP.combine` fname
    -- Mostly to filter out any directories (The node .. is always included for example)
    isFile <- FP.doesFileExist path
    case isFile of
      True -> do
        content <- BSL.readFile path
        time <- (`div` 1000) <$> currentPosixMillis
        pure $ Just $ Zip.toEntry fname (fromIntegral time) content
      False -> pure Nothing

  putStrLn $ "Archiving " ++ show (length entries) ++ " files"

  let archive = P.foldr Zip.addEntryToArchive Zip.emptyArchive (catMaybes entries)
  let archive_path = coreDumpArchive cv slug
  BSL.writeFile archive_path (Zip.fromArchive archive) 
  FP.makeAbsolute archive_path >>= \path -> putStrLn $ "Created " ++ path




