module PrettyPrint exposing (..)

import Types exposing (..)
import Generated.Types as H
import HsCore.Helpers as H
import State exposing (State)
import State as S

import Dict exposing (Dict)
import Char
import Either exposing (Either(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Reader exposing (Reader(..))

type alias PPEnv = { selectId : Maybe Int
                   }

type alias PPM a = Reader PPEnv a

type alias PP = PPM ((List (List (Html Msg))) -> (List (List (Html Msg))))

prettyPrint : PPEnv -> PP -> List (Html Msg)
prettyPrint info pp = Reader.runReader info pp []
                    |> List.reverse
                    |> List.intersperse [text "\n"]
                    |> List.concat

defaultInfo : Maybe Int -> PPEnv
defaultInfo selectId = 
    { selectId = selectId
    }

binderIsSelected : PPEnv -> H.Binder -> Bool
binderIsSelected env b = Maybe.withDefault False <| (Maybe.map (\id -> id == H.binderToInt b) (env.selectId))

externalIsSelected : PPEnv -> H.ExternalName -> Bool
externalIsSelected env e = Maybe.withDefault False <| (Maybe.map (\id -> id == H.externalNameToInt e) (env.selectId))
        
ppWhen : Bool -> PP -> PP
ppWhen b pp = if b then pp else Reader.pure identity

ppSepped : String -> List PP -> PP
ppSepped s = ppIntercalate (emitText s)

ppIdentity : PP
ppIdentity = Reader.pure identity

ppIntercalate : PP -> List PP -> PP
ppIntercalate sep pps = case pps of
    (x :: []) -> x
    (x :: xs) -> ppSeq [x, sep, ppIntercalate sep xs]
    [] -> ppIdentity

runPP : PPEnv -> PP -> List (List (Html Msg))
runPP info pp = Reader.runReader info pp []


indented : PP -> PP
indented pp = Reader <| \info -> 
    let whitespace = String.fromList (List.repeat 4 ' ')
        block = List.map (\x -> text whitespace::x) (runPP info pp)
    in \acc -> block ++ acc

newline : PP
newline = Reader.pure <| \acc -> []::acc

ppLines : List PP -> PP
ppLines = ppIntercalate newline

ppSeq : List PP -> PP
ppSeq pps = Reader.foldM (<<) identity pps

emit : Html Msg -> PP
emit node = Reader.pure <| \acc -> case acc of
        x::xs -> (x++[node])::xs
        []   -> [[node]] 

emitText : String -> PP
emitText msg = emit (text msg)

emitSpan : String -> String -> PP
emitSpan c msg = emit (span [class c] [text msg])

emitKeyword : String -> PP
emitKeyword = emitSpan "k"

parens : PP -> PP
parens pp = ppSeq [emitText "(", pp, emitText ")"]

parensExpr : H.Expr -> PP
parensExpr expr = case expr of
    H.EVar b -> ppExpr (H.EVar b)
    H.EVarGlobal b -> ppExpr (H.EVarGlobal b)
    H.ELit l -> ppExpr (H.ELit l)
    H.EType t -> ppExpr (H.EType t)
    _        -> parens (ppExpr expr)

ppType : H.Type -> PP
ppType t = emitText (H.typeToString t)

parensType : H.Type -> PP
parensType type_ = case type_ of
    H.VarTy t -> ppType (H.VarTy t)
    H.TyConApp con [] -> ppType (H.TyConApp con [])
    H.TyConApp (H.TyCon "[]" u) ts -> ppType (H.TyConApp (H.TyCon "[]" u) ts)
    _         -> parens (ppType type_)

ppLit : H.Lit -> PP
ppLit lit  = case lit of
    H.MachChar c -> emitSpan "s" (String.fromList ['\'', c, '\''])
    H.MachStr s  -> emitSpan "s" ("" ++ s ++ "")
    H.MachNullAddr -> emitKeyword "NullAddr#"
    H.MachInt i  -> emitSpan "m" i
    H.MachInt64 i -> emitSpan "m" i
    H.MachWord i -> emitSpan "m" i
    H.MachWord64 i -> emitSpan "m" i
    H.MachFloat f -> emitSpan "m" f
    H.MachDouble d -> emitSpan "m" d
    H.MachLabel l -> emitText l
    H.LitInteger i  -> emitSpan "m" i
    H.LitNatural n -> emitSpan "m" n
    H.LitRubbish -> emitText "[LitRubbish]"

ppBinderClass : String -> H.Binder -> PP
ppBinderClass c b = Reader.ask 
    |> Reader.andThen (\env ->
        emit <| a [ class "no-style"
                  , onClick (MsgSelectTerm (SelectedBinder b)) 
                  ]
                  [ span [ class c
                         , class (if binderIsSelected env b then "highlight" else "")
                         ] 
                         [text (H.binderName b)] 
                  ])

ppBinder : H.Binder -> PP
ppBinder b = ppBinderClass (if H.isConstructorName (H.binderName b) then "k" else "") b

ppBinderM : Maybe H.Binder -> PP
ppBinderM mb = case mb of
    Just b -> ppBinder b
    Nothing -> emitText "[!UKNOWN VARIABLE!]"

ppBinderT : H.BinderThunk -> PP
ppBinderT mb = case mb of
    H.Found b -> ppBinder b
    H.NotFound -> emitText "[!UKNOWN VARIABLE!]" 
    H.Untouched -> emitText "[!I WAS NEVER TOUCHED!]"

ppTopBinding : H.TopBinding -> PP
ppTopBinding b = case b of
    H.NonRecTopBinding bndr _ e -> ppBinding True (bndr, e)
    H.RecTopBinding xs -> Debug.todo "Recursive bindings not implemented"


ppBinding : Bool -> (H.Binder, H.Expr) -> PP
ppBinding toplevel (b, e) = 
    let (fe, bs) = H.leadingLambdas e
        in ppSeq [ if toplevel
                   then ppSeq [ ppBinderClass "nf" b
                              , emitText " :: "
                              , ppType (H.binderType b)
                              , newline
                              ]
                   else emitText ""
                 , ppBinderClass (if toplevel then "nf" else "") b
                 , emitText " "
                 , ppSepped " " (List.map ppBinder bs)
                 , emitText (if List.isEmpty bs then "" else " ")
                 , emitText "= ", ppExpr fe]

ppUnique : H.Unique -> PP
ppUnique (H.Unique _ i) = emitText (String.fromInt i)

ppExpr : H.Expr -> PP
ppExpr expr = case expr of
    H.EVar (H.BinderId _ getBinder) -> ppBinderT (getBinder ())
    H.EVarGlobal name -> ppExternalName name
    H.ELit lit -> ppLit lit
    H.ETyLam b e -> ppExpr (H.ELam b e)
    H.EApp f a -> ppSeq [ppExpr f, emitText " ", parensExpr a]
    H.ELam b e -> ppSeq [emitText "\\", ppBinder b, emitText " -> ", indented (ppExpr e)]
    H.ELet bs e ->  ppSeq [ emitKeyword "let "
                          , indented <| ppLines (List.map (ppBinding False) bs)
                          , newline
                          , emitKeyword " in ", ppExpr e
                          ]
    H.ECase e b alts -> ppCase e b alts
    H.EType t -> ppSeq [emitText "@", parensType t]
    _ -> emitText "[Expr TODO]"

ppCase : H.Expr -> H.Binder -> List H.Alt -> PP
ppCase e b alts = ppSeq [ emitKeyword "case "
                        , ppExpr e
                        , emitKeyword " of"
                        , indented <| 
                            ppSeq (List.map (\alt -> ppSeq [ppAlt alt, newline]) alts)
                        , newline
                        ]

ppAlt : H.Alt -> PP
ppAlt alt = ppSeq [ ppAltCon alt.altCon
                  , ppWhen (not (List.isEmpty alt.altBinders)) (emitText " ")
                  , ppSepped " " (List.map ppBinder alt.altBinders)
                  , emitText " -> "
                  , ppExpr alt.altRHS
                  ]

ppAltCon : H.AltCon -> PP
ppAltCon con = case con of
    H.AltDataCon s -> if H.isConstructorName s then emitKeyword s else emitText s
    H.AltLit l -> ppLit l
    H.AltDefault -> emitText "_"

ppExternalName : H.ExternalName -> PP
ppExternalName name = case name of 
    H.ExternalName e -> case e.localBinder () of
        H.Found b -> ppBinder b
        _         -> ppActualExternalName name
    H.ForeignCall -> ppActualExternalName name

ppActualExternalName : H.ExternalName -> PP
ppActualExternalName e = 
    let go env = let classes = [ class (if H.isConstructorName (H.externalName e) then "k" else "")
                               , class (if externalIsSelected env e then "highlight" else "") 
                               ] 
                 in emit (a [ class "no-style"
                            , onClick (MsgSelectTerm (SelectedExternal e))
                            ] 
                            [span classes [text (H.externalName e)]])
    in Reader.exec go



