module Core.Generated.Types exposing (..)

import Time exposing (Posix)
import Json.Decode exposing (Value)


type Unique
    = Unique Int

unUnique : Unique -> Int
unUnique (Unique x) = x
