module Pages.Code exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlHelpers exposing (..)
import Dict exposing (Dict)
import Generated.Types exposing (..)
import Types exposing (..)
import HsCore.Helpers as H
import HsCore.Trafo.Reconstruct as TR
import HsCore.Trafo.EraseTypes exposing (eraseTypesModule)
import PrettyPrint as PP
import Commands as C
import Loading exposing (Loading(..))
import Commands

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block

mkCodeMsg : CodeTabMsg -> TabId -> Msg
mkCodeMsg msg id = MsgCodeMsg id msg

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

makeCodeTab : Model -> List ProjectMeta -> (Model, CodeTab, Cmd Msg)
makeCodeTab model metas = 
    let tabId = model.idGen
        slugs = List.map .slug metas
        slugsandmetas = H.zip slugs metas
        
    in
    ( { model | idGen = model.idGen + 1 }
    , { id = tabId
      , name = "Code-" ++ String.fromInt tabId
      , modules = Dict.fromList (List.map (\s -> (s, Loading Nothing)) slugs)
      , projectMetas = Dict.fromList slugsandmetas
      , selectedTerm = Nothing
      , hideTypes = False
      , disambiguateVariables = False
      , currentModule = "Main"
      , currentPhaseId = 0
      }
    , Cmd.batch (List.map (\slug -> C.fetchCodePhase tabId slug "Main" 0) slugs)
    )

update : CodeTabMsg -> CodeTab -> (CodeTab, Cmd Msg)
update msg tab = case msg of
    CodeMsgSetModule modname phaseid -> 
        ( {tab | modules = Dict.map (\_ _ -> Loading Nothing) tab.modules 
               , currentModule = modname
               , currentPhaseId = phaseid
          }
        , Cmd.batch 
            ( List.map
             (\slug -> Commands.fetchCodePhase tab.id slug modname phaseid)
             (Dict.keys tab.modules)
            )
        )
    CodeMsgGotModule slug res -> ({tab | modules = Dict.insert slug (Loading.loadFromResult res) tab.modules}, Cmd.none)
    CodeMsgSelectTerm term -> ({tab | selectedTerm = Just term}, Cmd.none)
    CodeMsgToggleHideTypes -> ({tab | hideTypes = not tab.hideTypes}, Cmd.none)
    CodeMsgToggleDisambiguateVariables -> ({tab | disambiguateVariables = not tab.disambiguateVariables}, Cmd.none)


view : Model -> CodeTab -> Html Msg
view model tab = 
    div [] [ viewHeader model tab
           , panel
                (
                    (foreach (Dict.toList tab.modules) <| \(slug, mod) ->
                        (Fraction 4, viewCode model tab slug mod)
                    )
                    ++
                    [ (Pixels 500, viewInfo model tab) ]
                )
           ]


selectedTermId : CodeTab -> Maybe Int
selectedTermId tab = Maybe.map selectedTermToInt tab.selectedTerm


viewHeader : Model -> CodeTab -> Html Msg
viewHeader _ tab = 
    div []
        [ h3 []  [text (tab.currentModule ++ " - " ++ String.fromInt tab.currentPhaseId)]
        , button [onClick (mkCodeMsg (CodeMsgSetModule tab.currentModule (tab.currentPhaseId - 1)) tab.id)] [text "Previous"]
        , button [onClick (mkCodeMsg (CodeMsgSetModule tab.currentModule (tab.currentPhaseId + 1)) tab.id)] [text "Next"]
        ]



viewCode : Model -> CodeTab -> Slug -> Loading Module -> Html Msg
viewCode model tab slug modloading = 
    let ppInfo = PP.defaultInfo tab.id
            |> \r -> {r | selectId = Maybe.map selectedTermToInt tab.selectedTerm}
            |> if tab.disambiguateVariables then PP.withFullNameBinder else identity
    in div []
        [ h4 [] [text slug]
        , pre [class "dark"] 
            [ code [] 
                   [Loading.renderLoading modloading <| \mod ->
                       (
                         (if tab.hideTypes then eraseTypesModule mod else mod)
                         |> .moduleTopBindings
                         |> List.map PP.ppTopBinding
                         |> PP.ppSepped "\n\n"
                         |> PP.prettyPrint ppInfo
                         |> \x -> div [] x
                       )
                   ]
            ]
        ]

fromMaybe : a -> Maybe a -> a
fromMaybe def m = case m of
    Just x -> x
    Nothing -> def

viewInfo : Model -> CodeTab -> Html Msg
viewInfo model tab = 
    Card.config []
    |> Card.headerH3 [] [text "Options"]
    |> Card.block []
        [ Block.titleH4 [] [text "View Options"]
        , Block.custom <|
            HtmlHelpers.list 
              [ checkbox tab.hideTypes (mkCodeMsg CodeMsgToggleHideTypes tab.id) "Hide Types"
              , checkbox tab.disambiguateVariables (mkCodeMsg CodeMsgToggleDisambiguateVariables tab.id) "Disambiguate Variables Names"
              , hr [] []
              , h4 [] [text "Selected Variable"]
              , fromMaybe (h5 [] [text "No term selected"]) (Maybe.map viewTermInfo tab.selectedTerm)
              ]
        ]
    |> Card.view

viewTermInfo : SelectedTerm -> Html Msg
viewTermInfo term = case term of
    SelectedBinder b -> viewTermBinder b
    SelectedTopLevel (b,s) -> div [] [viewTermBinder b, br [] [], text (Debug.toString s)]
    SelectedExternal (ExternalName e) -> p [] [ text (H.typeToString e.externalType) ]
    SelectedExternal ForeignCall -> p [] [text "ForeignCall"]

viewTermBinder : Binder -> Html Msg
viewTermBinder bndr = HtmlHelpers.list
            [ text ("name: " ++ H.binderName bndr)
            , text ("type: " ++ H.typeToString (H.binderType bndr))
            , text ("span: " ++ Debug.toString (H.binderSpan bndr))
            ]



