module Core.Generated.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import Core.Generated.ElmStreet exposing (..)
import Core.Generated.Types as T


decodeUnique : Decoder T.Unique
decodeUnique = D.map T.Unique D.int
