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

import ViewCore
import ViewPanel



import Core.Generated.Encoder exposing (encodePassInfo)
import Core.Generated.Decoder exposing (decodeModuleInfo, decodeMetaInfo)
import Core.Generated.Types exposing (..)
import CoreLangUtils exposing (..)

import List
import PprCoreLang exposing (..)
import Trafo


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

fetchModule : String -> Cmd Msg
fetchModule mod = Http.get { url = "http://127.0.0.1:8080/core/" ++ mod
                           , expect = Http.expectJson MsgGotModule decodeModuleInfo
                           }

fetchSrc : String -> Cmd Msg
fetchSrc mod = Http.get { url = "http://127.0.0.1:8080/source/" ++ mod
                        , expect = Http.expectString MsgGotSrc
                        }

fetchMeta : Cmd Msg
fetchMeta = Http.get { url = "http://127.0.0.1:8080/meta"
                     , expect = Http.expectJson MsgGotMeta decodeMetaInfo
                     }



getModule : Model -> Maybe ModuleInfo
getModule model = loadToMaybe model.moduleLoading

getMetaModules : Model -> List String
getMetaModules model = case model.metaLoading of
    Ready meta -> meta.modules
    _          -> []

initModel : Model
initModel = { moduleLoading = Loading Nothing
            , srcLoading = Loading Nothing
            , metaLoading = Loading Nothing
            , shownBindings = Set.empty
            , showTypeApplications = True
            , showBndrTypes = False
            , showUniqueName = False
            , selectedTerm = Nothing
            , renames = Dict.empty
            , showSource = True
            , currentPass = 1
            }



init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.batch [fetchSrc "Main", fetchModule "Main", fetchMeta])

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    MsgFetchModule mod -> 
        let prevModule = getModule model
        in ( { model | moduleLoading = Loading prevModule }
           , Cmd.batch [fetchModule mod, fetchSrc mod]
           )
    MsgGotModule result -> ( { model | moduleLoading = loadFromResult result } , Cmd.none)
    MsgFetchSrc mod -> ({model | srcLoading = Loading Nothing }, fetchSrc mod)
    MsgGotSrc result -> ({model | srcLoading = loadFromResult result }, Cmd.none)
    MsgFetchMeta -> ({model | metaLoading = Loading Nothing}, fetchMeta)
    MsgGotMeta result -> ({model | metaLoading = loadFromResult result}, Cmd.none)
    MsgNextPass -> ({model | currentPass = model.currentPass + 1}, Cmd.none)
    MsgPrevPass -> ({model | currentPass = model.currentPass - 1}, Cmd.none)
    MsgToggleHiddenBind bind -> ( { model | shownBindings = toggleSet bind model.shownBindings} , Cmd.none)
    MsgHideAllBinds -> ( { model | shownBindings = Set.empty } , Cmd.none)
    MsgSelectTerm term -> ( { model | selectedTerm = Just term } , Cmd.none)
    MsgRenameTerm unique name -> ( { model | renames = Dict.insert unique name model.renames } , Cmd.none)
    MsgToggleShowTypeApps -> ( { model | showTypeApplications = not model.showTypeApplications } , Cmd.none)
    MsgToggleShowBndrTypes -> ( {model | showBndrTypes = not model.showBndrTypes }, Cmd.none)
    MsgToggleUniqueName -> ( { model | showUniqueName = not model.showUniqueName } , Cmd.none)
    MsgToggleShowSource -> ( {model | showSource = not model.showSource  }, Cmd.none)


                         



-- Toggles membership of a set
toggleSet : comparable -> Set comparable -> Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set

applyRenames : Model -> PassInfo -> PassInfo
applyRenames model pass = {pass | binds = List.map (Trafo.applyRenames model.renames) pass.binds}

eraseTypes : Model -> PassInfo -> PassInfo
eraseTypes model pass = if model.showTypeApplications 
                        then pass
                        else { pass | binds = List.map (Trafo.eraseTypes) pass.binds }

preparePass : Model -> PassInfo -> PassInfo
preparePass model = applyRenames model << eraseTypes model

tryViewSrc : Model -> Html Msg
tryViewSrc model = case model.srcLoading of
    Ready src -> pre [class "code"] [Markdown.toHtml [] ("```haskell\n" ++ src ++ "\n```")]
    Loading _ -> text "Loading"
    Failure _ -> text "Source not available"

moduleDropDown : Model -> PassInfo -> Html Msg
moduleDropDown model passInfo =
    let inputEvent modName = MsgFetchModule modName
        makeOption modName = option [selected (modName == passInfo.modname)] [text modName]

    in select [onInput inputEvent] (List.map makeOption (getMetaModules model))

indexList : List a -> Int -> Maybe a
indexList l n = case (l, n) of
    ([], _) -> Nothing
    (x::_, 0) -> Just x
    (_::xs, _) -> indexList xs (n - 1)

view : Model -> Html Msg
view model = 
    let m_mod = loadToMaybe model.moduleLoading
        m_pass = m_mod |> Maybe.andThen (\m -> indexList m.passes (model.currentPass - 1)) 
                       |> Maybe.map (preparePass model)
    in case (m_mod, m_pass) of
        (Nothing, _) -> text "Module not loaded"
        (_, Nothing) -> text "Pass not found"
        (Just mod, Just pass) -> 
            div [] 
                [ h1 [] [text (String.fromInt model.currentPass ++ ": " ++ pass.title)]
                , br [] []
                , button [onClick MsgToggleShowSource] [text "Toggle source"]
                , button [onClick MsgPrevPass] [text "Previous"]
                , button [onClick MsgNextPass] [text "Next"]
                , moduleDropDown model pass
                , div (panelStyle model)
                      [ if model.showSource then tryViewSrc model else text ""
                      , ViewCore.view model pass
                      , ViewPanel.view model mod pass
                      ]
                ]

panelStyle : Model -> List (Attribute Msg)
panelStyle model = 
    [ style "display" "grid"
    , style "width" "100%"
    , if model.showSource
      then style "grid-template-columns" "2fr 2fr 1fr"
      else style "grid-template-columns" "4fr 1fr"
    ]






