module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Http
import Set exposing (Set)

import Json.Print
import Json.Encode
import Json.Decode


import Core.Generated.Encoder exposing (encodeCoreBind)
import Core.Generated.Decoder exposing (decodePassInfo)
import Core.Generated.Types exposing (..)

import List
import PprCoreLang exposing (..)

type PassLoading = Loading (Maybe PassInfo) | Failure Http.Error | Ready PassInfo

type alias Model = { passLoading : PassLoading
                   , hiddenBindings : Set String
                   }
type Msg = GotPass (Result Http.Error PassInfo)
         | FetchPass Int
         | ToggleHiddenBind String


main : Program () Model Msg
main = Browser.element { init = init
                       , update = update
                       , subscriptions = subscriptions
                       , view = view 
                       }


jsonToString : Json.Encode.Value -> String
jsonToString json = Result.withDefault "" (Json.Print.prettyString (Json.Print.Config 4 50) (Json.Encode.encode 0 json))

fetchPass : Int -> Cmd Msg
fetchPass idx = Http.get { url = "http://127.0.0.1:8080/" ++ String.fromInt idx
                         , expect = Http.expectJson GotPass decodePassInfo
                         }

initModel : Model
initModel = { passLoading = Loading Nothing
            , hiddenBindings = Set.empty
            }

init : () -> (Model, Cmd Msg)
init _ = (initModel, fetchPass 1)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    GotPass result -> case result of
        Ok pass -> ({ model | passLoading = Ready pass }, Cmd.none)
        Err e -> ({ model | passLoading = Failure e}, Cmd.none)
    FetchPass idx -> 
        let prevPass = case model.passLoading of
                Ready pass -> Just pass
                _          -> Nothing
        in ( {model | passLoading = Loading prevPass }
           , fetchPass idx
           )
    ToggleHiddenBind bind -> ( { model | hiddenBindings = toggleSet bind model.hiddenBindings}
                             , Cmd.none
                             )

checkbox : msg -> String -> Html msg
checkbox msg name =
    label
        [ ]
        [ input [ type_ "checkbox", onClick msg ] []
        , text name
        ]

-- Toggles membership of a set
toggleSet : comparable -> Set comparable -> Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set


view : Model -> Html Msg
view model = 
    let body = case model.passLoading of
            Loading Nothing -> text "loading pass "
            -- Pevent screen flashes
            Loading (Just prevPass) -> view ({model | passLoading = Ready prevPass})
            Failure _ -> text "Something went wrong"
            Ready pass -> 
                let binds = List.filter (\b -> not <| Set.member (coreBindName b) model.hiddenBindings) pass.binds
                in div []  [ h1 [] [text (String.fromInt pass.idx ++ ": " ++ pass.title)]
                           , button [onClick (FetchPass <| pass.idx - 1)] [text "Previous"]
                           , button [onClick (FetchPass <| pass.idx + 1)] [text "Next"]
                           , pre [class "code"] (List.concatMap viewCoreBind binds)
                           , viewHiddenList model pass
                           , pre [] (List.map (text << jsonToString << encodeCoreBind) pass.binds)
                           ]
    in div [] [ css "pygments.css"
              , css "style.css"
              , body
              ]

viewHiddenList : Model -> PassInfo -> Html Msg
viewHiddenList model pass = 
    let go bind = li [] [checkbox (ToggleHiddenBind bind) bind]

    in ul [] (List.map (go << coreBindName) pass.binds)


css : String -> Html Msg
css path = node "link" [ rel "stylesheet", href path ] []
