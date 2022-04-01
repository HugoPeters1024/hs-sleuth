module Main exposing (main)

import MsgTypes exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Http
import Set exposing (Set)
import Dict exposing (Dict)

import Json.Print
import Json.Encode
import Json.Decode


import Core.Generated.Encoder exposing (encodePassInfo)
import Core.Generated.Decoder exposing (decodePassInfo)
import Core.Generated.Types exposing (..)

import List
import PprCoreLang exposing (..)
import Trafo

type PassLoading = Loading (Maybe PassInfo) | Failure Http.Error | Ready PassInfo

type alias Model = { passLoading : PassLoading
                   , hiddenBindings : Set String
                   , showTypeApplications : Bool
                   , selectedTerm : Maybe CoreId
                   , renames : Dict String String
                   }

main : Program () Model Msg
main = Browser.element { init = init
                       , update = update
                       , subscriptions = subscriptions
                       , view = view 
                       }

maybeHtml : (a -> Html msg) -> Maybe a -> Html msg
maybeHtml f mb = case mb of
    Just x -> f x
    Nothing -> text ""


jsonToString : Json.Encode.Value -> String
jsonToString json = Result.withDefault "" (Json.Print.prettyString (Json.Print.Config 4 50) (Json.Encode.encode 0 json))

fetchPass : Int -> Cmd Msg
fetchPass idx = Http.get { url = "http://127.0.0.1:8080/" ++ String.fromInt idx
                         , expect = Http.expectJson MsgGotPass decodePassInfo
                         }

getPass : Model -> Maybe PassInfo
getPass model = case model.passLoading of
    Ready p -> Just p
    _       -> Nothing

initModel : Model
initModel = { passLoading = Loading Nothing
            , hiddenBindings = Set.empty
            , showTypeApplications = True
            , selectedTerm = Nothing
            , renames = Dict.empty
            }


init : () -> (Model, Cmd Msg)
init _ = (initModel, fetchPass 1)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    MsgGotPass result -> case result of
        Ok pass -> ( { model | passLoading = Ready pass }
                   , Cmd.none
                   )
        Err e   -> ( { model | passLoading = Failure e}
                   , Cmd.none
                   )
    MsgFetchPass idx -> 
        let prevPass = case model.passLoading of
                Ready pass -> Just pass
                _          -> Nothing

        in ( { model | passLoading = Loading prevPass }
           , fetchPass idx
           )
    MsgToggleHiddenBind bind -> ( { model | hiddenBindings = toggleSet bind model.hiddenBindings}
                                , Cmd.none
                                )
    MsgHideAllBinds -> case getPass model of
        Just pass -> ( { model 
                       | hiddenBindings = Set.union model.hiddenBindings (Set.fromList (List.map coreBindBndrUnique pass.binds)) 
                       }
                     , Cmd.none
                     )
        Nothing   -> (model, Cmd.none)
    MsgSelectTerm term -> ( { model | selectedTerm = Just term }
                          , Cmd.none
                          )
    MsgRenameTerm unique name -> ( { model | renames = Dict.insert unique name model.renames }
                                 , Cmd.none
                                 )
    MsgToggleViewTypes -> ( { model | showTypeApplications = not model.showTypeApplications } 
                          , Cmd.none
                          )
                         


checkbox : Bool -> msg -> String -> Html msg
checkbox isChecked msg name =
    label
        [ ]
        [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
        , text name
        ]

-- Toggles membership of a set
toggleSet : comparable -> Set comparable -> Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set

prepareModelView : Model -> Model
prepareModelView model =
    let applyRenames : PassLoading -> PassLoading
        applyRenames loading = case loading of
            Ready pass -> Ready {pass | binds = List.map (Trafo.applyRenames model.renames) pass.binds}
            _ -> loading
    in { model | passLoading = applyRenames model.passLoading}


view : Model -> Html Msg
view rawmodel = 
    let model = prepareModelView rawmodel
        body = case model.passLoading of
            Loading Nothing -> text "loading pass "
            -- Pevent screen flashes
            Loading (Just prevPass) -> view ({model | passLoading = Ready prevPass})
            Failure err -> case err of
                Http.BadUrl _ -> text "Bad url"
                Http.Timeout -> text "timeout"
                Http.NetworkError -> text "network error"
                Http.BadStatus _ -> text "bad status"
                Http.BadBody _ -> text "bad body"



            Ready pass -> 
                let binds = List.filter (\b -> not <| Set.member (coreBindBndrUnique b) model.hiddenBindings) pass.binds
                    viewBind = PprCoreLang.viewCoreBind model.showTypeApplications model.selectedTerm
                in div []  [ h1 [] [text (String.fromInt pass.idx ++ ": " ++ pass.title)]
                           , br [] []
                           , button [onClick (MsgFetchPass <| pass.idx - 1)] [text "Previous"]
                           , button [onClick (MsgFetchPass <| pass.idx + 1)] [text "Next"]
                           , div [ class "panel-4-1" ] 
                                 [ pre [class "code"] (List.concatMap viewBind binds)
                                 , div [class "info-panel"] 
                                    [ viewDisplayOptions model
                                    , hr [] []
                                    , viewTermInfo model
                                    , hr [] []
                                    , viewHiddenList model pass
                                    ]
                                 ]
--                           , pre [] [text ((jsonToString (encodePassInfo pass)))]
                           ]
    in div [] [ css "pygments.css"
              , css "style.css"
              , css "https://fonts.googleapis.com/css?family=Ubuntu"
              , body
              ]

isHidden : Model -> CoreId -> Bool
isHidden model bind = Set.member bind.unique model.hiddenBindings

viewDisplayOptions : Model -> Html Msg
viewDisplayOptions model = div []
    [ h2 [] [text "Options"]
    , checkbox (model.showTypeApplications) MsgToggleViewTypes "Show type applications"
    ]

viewHiddenList : Model -> PassInfo -> Html Msg
viewHiddenList model pass = 
    let go : CoreBind -> Html Msg
        go bind = li [] [checkbox (isHidden model (coreBindBndr bind)) (MsgToggleHiddenBind (coreBindBndrUnique bind)) (coreBindBndrName bind ++ "_" ++ coreBindBndrUnique bind)]

    in div [ class "hidden-fields"] 
           [ h2 [] [text "Functions to hide"]
           , button [onClick MsgHideAllBinds] [text "hide all" ]
           , ul [class "no-dot"] (List.map go pass.binds) 
           ]

viewTermInfo : Model -> Html Msg
viewTermInfo model =
    let showMenu : CoreId -> Html Msg
        showMenu id =
            ul [class "no-dot"] [ li [] [ text ("name: " ++ id.name) ]
                                , li [] [ text ("type: "), span [class "kt"] [text id.vartype]]
                                , li [] [ text ("udi: " ++ id.unique)]
                                , li [] [ input [ type_ "text", placeholder id.name, onInput (MsgRenameTerm id.unique)] [] ]
                                , li [] [ checkbox (isHidden model id) (MsgToggleHiddenBind id.unique) "hidden" ]
                                ]
    in div [ class "term-info" ]
           [ h2 [] [text "Selected term"]
           , Maybe.withDefault (text "Nothing selected") (Maybe.map showMenu model.selectedTerm)
           ]
   
css : String -> Html Msg
css path = node "link" [ rel "stylesheet", href path ] []
