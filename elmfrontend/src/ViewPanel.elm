module ViewPanel exposing (..)

import Set exposing (Set)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import MsgTypes exposing (..)
import Core.Generated.Types exposing (..)
import CoreLangUtils exposing (..)
import Trafo

view : Model -> ModuleInfo -> PassInfo -> Html Msg
view model mod pass = div [class "info-panel"] 
                      [ viewDisplayOptions model
                      , hr [] []
                      , viewTermInfo model pass
                      , hr [] []
                      , viewHiddenList model mod pass
                      ]

checkbox : Bool -> msg -> String -> Html msg
checkbox isChecked msg name =
    label
        [ ]
        [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
        , text name
        ]

isShown : Model -> CoreId -> Bool
isShown model var = Set.member var.unique model.shownBindings

viewShownCheckbox : Model -> CoreId -> Html Msg
viewShownCheckbox model bndr = checkbox (isShown model bndr) (MsgToggleHiddenBind bndr.unique) (bndr.name ++ "_" ++ bndr.uniquetag)

viewDisplayOptions : Model -> Html Msg
viewDisplayOptions model = div []
    [ h2 [] [text "Options"]
    , ul [class "no-dot"]
         [ li [] [checkbox (model.showTypeApplications) MsgToggleShowTypeApps "Show type applications"]
         , li [] [checkbox (model.showBndrTypes) MsgToggleShowBndrTypes "Show binder types"]
         , li [] [checkbox (model.showUniqueName) MsgToggleUniqueName "Disambiguate variables"]
         ]
    ]

viewHiddenList : Model -> ModuleInfo -> PassInfo -> Html Msg
viewHiddenList model mod pass = 
    let go : CoreBind -> Html Msg
        go bind = 
            let lis = liSet bind
            in li [] [ if List.length lis == 0
                       then viewShownCheckbox model (coreBindBndr bind)
                       else details [style "margin-left" "-16px"] [ summary [] [viewShownCheckbox model (coreBindBndr bind)]
                                       , ul [class "no-dot"] (liSet bind)
                                       ]
                     ]

        liSet : CoreBind -> List (Html Msg)
        liSet bind = 
            let childrenIds = List.filter isTopLevel (Trafo.collectAllVarsBind bind)
            in List.map (\id -> li [] [viewShownCheckbox model id]) childrenIds

        topLevelSet : Set Int
        topLevelSet = Set.fromList <| List.map coreBindBndrUnique pass.binds

        isTopLevel : CoreId -> Bool
        isTopLevel var = Set.member var.unique topLevelSet

        srcSet : Set Int
        srcSet = Set.fromList mod.srcbindings

        isSrc : CoreBind -> Bool
        isSrc var = Set.member (coreBindBndrUnique var) srcSet

    in div [ class "hidden-fields"] 
           [ h2 [] [text "Functions to show"]
           , button [onClick MsgHideAllBinds] [text "hide all" ]
           , h3 [] [text "Source binds"]
           , ul [class "no-dot"] (List.map go (List.filter isSrc pass.binds))
           , h3 [] [text "Others"]
           , ul [class "no-dot"] (List.map go (List.filter (not << isSrc) pass.binds))
           ]

-- O(n), use sparingly
isTopLevelSlow : Model -> PassInfo -> CoreId -> Bool
isTopLevelSlow model pass bndr = List.member bndr.unique (List.map coreBindBndrUnique pass.binds)

viewTermInfo : Model -> PassInfo -> Html Msg
viewTermInfo model pass =
    let showMenu : CoreId -> Html Msg
        showMenu id =
            ul [class "no-dot"] [ li [] [ text ("name: " ++ id.name) ]
                                , li [] [ text ("type: "), span [class "kt"] [text id.vartype]]
                                , li [] [ text ("tag: " ++ id.uniquetag)]
                                , li [] [ text ("udi: " ++ String.fromInt id.unique)]
                                , li [] [ input [ type_ "text", placeholder id.name, onInput (MsgRenameTerm id.unique)] [] ]
                                , if isTopLevelSlow model pass id 
                                  then li [] [ checkbox (isShown model id) (MsgToggleHiddenBind id.unique) "shown" ]
                                  else text ""
                                ]
    in div [ class "term-info" ]
           [ h2 [] [text "Selected term"]
           , Maybe.withDefault (text "Nothing selected") (Maybe.map showMenu model.selectedTerm)
           ]
