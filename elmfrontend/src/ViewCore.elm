module ViewCore exposing (..)

import List
import Set exposing (Set)
import Html exposing (..)
import Html.Attributes exposing (..)

import Core.Generated.Types exposing (..)
import PprCoreLang 

import CoreLangUtils exposing (..)

import MsgTypes exposing (..)

view : Model -> PassInfo -> Html Msg
view model pass =
    let binds = List.filter (\b -> Set.member (coreBindBndrUnique b) model.shownBindings) pass.binds
        viewBind = PprCoreLang.viewCoreBind model.showUniqueName model.showBndrTypes model.selectedTerm
    in pre [class "code"] (List.concatMap viewBind binds)

