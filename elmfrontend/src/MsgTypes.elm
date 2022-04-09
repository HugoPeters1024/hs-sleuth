module MsgTypes exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import Http
import Core.Generated.Types exposing (..)

type Msg = MsgFetchModule String
         | MsgGotModule (Result Http.Error ModuleInfo)
         | MsgFetchSrc String
         | MsgGotSrc (Result Http.Error String)
         | MsgFetchMeta
         | MsgGotMeta (Result Http.Error MetaInfo)
         | MsgFetchPass String Int
         | MsgGotPass (Result Http.Error PassInfo)
         | MsgNextPass
         | MsgPrevPass
         | MsgToggleHiddenBind Int
         | MsgHideAllBinds
         | MsgSelectTerm CoreId
         | MsgRenameTerm Int String
         | MsgToggleShowTypeApps
         | MsgToggleShowBndrTypes
         | MsgToggleUniqueName
         | MsgToggleShowSource

type Loading a = Loading (Maybe a) | Failure Http.Error | Ready a

type alias Model = { moduleLoading : Loading ModuleInfo
                   , srcLoading : Loading String
                   , metaLoading : Loading MetaInfo
                   , passLoading : Loading PassInfo
                   , shownBindings : Set Int
                   , showTypeApplications : Bool
                   , showBndrTypes : Bool
                   , showUniqueName : Bool
                   , selectedTerm : Maybe CoreId
                   , renames : Dict Int String
                   , showSource : Bool
                   }

loadFromResult : Result Http.Error a -> Loading a
loadFromResult result = case result of
    Ok el -> Ready el
    Err e -> Failure e

setLoading : Loading a -> Loading a
setLoading loading = case loading of
    Ready x -> Loading (Just x)
    _       -> Loading Nothing

loadToMaybe : Loading a -> Maybe a
loadToMaybe loading = case loading of
    Ready x -> Just x
    Loading x -> x
    _       -> Nothing

whenLoaded : b -> (a -> b) -> Loading a -> b
whenLoaded def f loading = case loading of
    Ready x -> f x
    _ -> def
