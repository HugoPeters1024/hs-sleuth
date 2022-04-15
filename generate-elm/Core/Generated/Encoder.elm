module Core.Generated.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import Core.Generated.ElmStreet exposing (..)
import Core.Generated.Types as T


encodeUnique : T.Unique -> Value
encodeUnique = E.int << T.unUnique
