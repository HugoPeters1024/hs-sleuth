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

import Markdown


import Core.Generated.Encoder exposing (encodePassInfo)
import Core.Generated.Decoder exposing (decodePassInfo)
import Core.Generated.Types exposing (..)

import List
import PprCoreLang exposing (..)
import Trafo

type Loading a = Loading (Maybe a) | Failure Http.Error | Ready a

type alias Model = { passLoading : Loading PassInfo
                   , srcLoading : Loading String
                   , shownBindings : Set Int
                   , showTypeApplications : Bool
                   , showUniqueName : Bool
                   , selectedTerm : Maybe CoreId
                   , renames : Dict Int String
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
fetchPass idx = Http.get { url = "http://127.0.0.1:8080/core/" ++ String.fromInt idx
                         , expect = Http.expectJson MsgGotPass decodePassInfo
                         }

fetchSrc : String -> Cmd Msg
fetchSrc mod = Http.get  { url = "http://127.0.0.1:8080/source/Main"
                         , expect = Http.expectString MsgGotSrc
                         }

getPass : Model -> Maybe PassInfo
getPass model = case model.passLoading of
    Ready p -> Just p
    _       -> Nothing

initModel : Model
initModel = { passLoading = Loading Nothing
            , srcLoading = Loading Nothing
            , shownBindings = Set.empty
            , showTypeApplications = True
            , showUniqueName = False
            , selectedTerm = Nothing
            , renames = Dict.empty
            }

loadFromResult : Result Http.Error a -> Loading a
loadFromResult result = case result of
    Ok el -> Ready el
    Err e -> Failure e


init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.batch [fetchSrc "Main", fetchPass 1])

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    MsgFetchPass idx -> 
        let prevPass = case model.passLoading of
                Ready pass -> Just pass
                _          -> Nothing

        in ( { model | passLoading = Loading prevPass }
           , fetchPass idx
           )
    MsgGotPass result -> ( { model | passLoading = loadFromResult result } , Cmd.none)
    MsgFetchSrc mod -> ({model | srcLoading = Loading Nothing }, fetchSrc mod)
    MsgGotSrc result -> ({model | srcLoading = loadFromResult result }, Cmd.none)
    MsgToggleHiddenBind bind -> ( { model | shownBindings = toggleSet bind model.shownBindings}
                                , Cmd.none
                                )
    MsgHideAllBinds -> ( { model | shownBindings = Set.empty }
                       , Cmd.none
                       )
    MsgSelectTerm term -> ( { model | selectedTerm = Just term }
                          , Cmd.none
                          )
    MsgRenameTerm unique name -> ( { model | renames = Dict.insert unique name model.renames }
                                 , Cmd.none
                                 )
    MsgToggleViewTypes -> ( { model | showTypeApplications = not model.showTypeApplications } 
                          , Cmd.none
                          )
    MsgToggleUniqueName -> ( { model | showUniqueName = not model.showUniqueName } 
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
    let applyRenames : Loading PassInfo -> Loading PassInfo
        applyRenames loading = case loading of
            Ready pass -> Ready {pass | binds = List.map (Trafo.applyRenames model.renames) pass.binds}
            _ -> loading

        filterTypes : Loading PassInfo -> Loading PassInfo
        filterTypes loading = case loading of
            Ready pass -> if model.showTypeApplications
                             then Ready pass
                             else Ready { pass | binds = List.map (Trafo.eraseTypes) pass.binds }
            _ -> loading
                             
    in { model | passLoading = filterTypes (applyRenames model.passLoading) }

tryViewSrc : Model -> Html Msg
tryViewSrc model = case model.srcLoading of
    Ready src -> pre [class "code"] [Markdown.toHtml [] ("```haskell\n" ++ src ++ "\n```")]
    Loading _ -> text "Loading"
    Failure _ -> text "Source not available"



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
                let binds = List.filter (\b -> Set.member (coreBindBndrUnique b) model.shownBindings) pass.binds
                    viewBind = PprCoreLang.viewCoreBind model.showUniqueName model.selectedTerm
                in div []  [ h1 [] [text (String.fromInt pass.idx ++ ": " ++ pass.title)]
                           , br [] []
                           , button [onClick (MsgFetchPass <| pass.idx - 1)] [text "Previous"]
                           , button [onClick (MsgFetchPass <| pass.idx + 1)] [text "Next"]
                           , div [ class "panel-4-1" ] 
                                 [ tryViewSrc model
                                 , pre [class "code"] (List.concatMap viewBind binds)
                                 , div [class "info-panel"] 
                                    [ viewDisplayOptions model
                                    , hr [] []
                                    , viewTermInfo model
                                    , hr [] []
                                    , viewHiddenList model pass
                                    ]
                                 ]
                           , pre [] [text ((jsonToString (encodePassInfo pass)))]
                           ]
    in div [] [body
              , node "script" [src "highlight.min.js"] []
              , node "script" [] [text "hljs.highlightAll();"]
              , body
              ]

isShown : Model -> CoreId -> Bool
isShown model bind = Set.member bind.unique model.shownBindings

viewDisplayOptions : Model -> Html Msg
viewDisplayOptions model = div []
    [ h2 [] [text "Options"]
    , ul [class "no-dot"]
         [ li [] [checkbox (model.showTypeApplications) MsgToggleViewTypes "Show type applications"]
         , li [] [checkbox (model.showUniqueName) MsgToggleUniqueName "Disambiguate variables"]
         ]
    ]

viewShownCheckbox : Model -> CoreBind -> Html Msg
viewShownCheckbox model bind = checkbox (isShown model (coreBindBndr bind)) (MsgToggleHiddenBind (coreBindBndrUnique bind)) (coreBindBndrName bind ++ "_" ++ coreBindBndrUniqueTag bind)

viewHiddenList : Model -> PassInfo -> Html Msg
viewHiddenList model pass = 
    let go : CoreBind -> Html Msg
        go bind = li [] [ details [] 
                                  [ summary [] [viewShownCheckbox model bind]
                                  , ul [class "no-dot"] [ li [] [text "work in progress"]
                                                        , li [] [text "and more"]
                                                        , li [] [checkbox True (MsgHideAllBinds) "dont click!"]
                                                        ] 
                                  ]
                        ]

    in div [ class "hidden-fields"] 
           [ h2 [] [text "Functions to show"]
           , button [onClick MsgHideAllBinds] [text "hide all" ]
           , ul [class "no-dot"] (List.map go pass.binds) 
           ]

viewTermInfo : Model -> Html Msg
viewTermInfo model =
    let showMenu : CoreId -> Html Msg
        showMenu id =
            ul [class "no-dot"] [ li [] [ text ("name: " ++ id.name) ]
                                , li [] [ text ("type: "), span [class "kt"] [text id.vartype]]
                                , li [] [ text ("tag: " ++ id.uniquetag)]
                                , li [] [ text ("udi: " ++ String.fromInt id.unique)]
                                , li [] [ input [ type_ "text", placeholder id.name, onInput (MsgRenameTerm id.unique)] [] ]
                                , li [] [ checkbox (isShown model id) (MsgToggleHiddenBind id.unique) "shown" ]
                                ]
    in div [ class "term-info" ]
           [ h2 [] [text "Selected term"]
           , Maybe.withDefault (text "Nothing selected") (Maybe.map showMenu model.selectedTerm)
           ]
