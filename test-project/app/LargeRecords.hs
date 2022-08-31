{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LargeRecords where

--import Data.Record.Plugin
--import Data.Record.Plugin.Options
--
--{-# ANN type B largeRecordStrict #-}
--data B = MkB { field00 :: Int
--             , field01 :: Int
--             , field02 :: Int
--             , field03 :: Int
--             , field04 :: Int
--             , field05 :: Int
--             , field06 :: Int
--             , field07 :: Int
--             , field08 :: Int
--             , field09 :: Int
--             , field10 :: Int
--             }
