port module Ports exposing (..)

port openBrowserTab : String -> Cmd msg

port triggerHighlight : () -> Cmd msg
