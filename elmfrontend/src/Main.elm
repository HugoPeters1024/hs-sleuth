module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, pre, p, br, node)
import Html.Attributes exposing (rel, href, class)
import Html.Events exposing (onClick)

import Http

import Json.Print
import Json.Encode
import Json.Decode


import Core.Generated.Encoder exposing (encodeCoreBind)
import Core.Generated.Decoder exposing (decodeCoreBind)
import Core.Generated.Types exposing (..)

import List
import PprCoreLang


type Model = Loading | Failure Http.Error | Ready (List CoreBind)
type Msg = GotText (Result Http.Error (List CoreBind))


main : Program () Model Msg
main = Browser.element { init = init
                       , update = update
                       , subscriptions = subscriptions
                       , view = view 
                       }


jsonToString : Json.Encode.Value -> String
jsonToString json = Result.withDefault "" (Json.Print.prettyString (Json.Print.Config 4 50) (Json.Encode.encode 0 json))


init : () -> (Model, Cmd Msg)
init _ = ( Loading
         , Http.get { url = "http://127.0.0.1:8080" 
                    , expect = Http.expectJson GotText (Json.Decode.list decodeCoreBind)
                    }
          )

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg _ = case msg of
    GotText result -> case result of
        Ok fullText -> (Ready fullText, Cmd.none)
        Err e -> (Failure e, Cmd.none)

view : Model -> Html Msg
view model = 
    let body = case model of
            Loading -> text "loading..."
            Failure _ -> text "Something went wrong"
            Ready binds -> div [] [ pre [class "code"] (List.concatMap PprCoreLang.viewCoreBind binds)
                              , pre [] (List.map (text << jsonToString << encodeCoreBind) binds)
                              ]
    in div [] [ css "pygments.css"
              , css "style.css"
              , body
              ]


css : String -> Html Msg
css path = node "link" [ rel "stylesheet", href path ] []
