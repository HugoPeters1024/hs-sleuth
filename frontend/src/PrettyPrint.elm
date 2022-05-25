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
                   , onClickBinder : Var -> Msg
                   , renderVarName : Var -> String
                   }

type alias PPM a = Reader PPEnv a

type alias PP = PPM ((List (List (Html Msg))) -> (List (List (Html Msg))))

prettyPrint : PPEnv -> PP -> List (Html Msg)
prettyPrint info pp = Reader.runReader info pp []
                    |> List.reverse
                    |> List.intersperse [text "\n"]
                    |> List.concat

defaultInfo : TabId -> PPEnv
defaultInfo tid = 
    { selectId = Nothing
    , onClickBinder = MsgCodeMsg tid << CodeMsgSelectVar
    , renderVarName = H.varName False
    }

withFullNameBinder : PPEnv -> PPEnv
withFullNameBinder env = { env | renderVarName = H.varName True }

varIsSelected : PPEnv -> Var -> Bool
varIsSelected env b = Maybe.withDefault False <| (Maybe.map (\id -> id == H.varToInt b) (env.selectId))

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
indented pp = ppSeq
    [ Reader <| \info -> 
        let whitespace = String.fromList (List.repeat 4 ' ')
            block = List.map (\x -> text whitespace::x) (runPP info pp)
        in \acc -> block ++ acc
    , newline
    ]

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

emitLine : String -> PP
emitLine t = ppSeq [newline, emitText t]

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

parensType : H.Type -> PP
parensType type_ = case type_ of
    H.VarTy t -> ppType (H.VarTy t)
    H.TyConApp con xs -> ppType (H.TyConApp con xs)
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

getVarClasses : Var -> PPM (List (Attribute msg))
getVarClasses var = Reader <| \env -> List.concat 
    [  if varIsSelected env var then [class "highlight"] else []
    ,  if H.varIsConstructor var then [class "k"] else []
    ,  if H.varIsTopLevel var then [class "nf"] else []
    ]

ppVar : Var -> PP
ppVar var = case H.varExternalLocalBinder var of
    Just b -> ppVar (VarBinder b)
    Nothing -> Reader.map2 (\env cs -> 
        a [ class "no-style"]
          [ span (onClick (env.onClickBinder var)::cs) [text (env.renderVarName var)]
          ]
        ) Reader.ask (getVarClasses var) |> Reader.andThen emit

ppBinder : H.Binder -> PP
ppBinder = ppVar << VarBinder

ppBinderT : H.BinderThunk -> PP
ppBinderT mb = case mb of
    H.Found b -> ppVar (VarBinder b)
    H.NotFound -> emitText "[!UKNOWN VARIABLE!]" 
    H.Untouched -> emitText "[!I WAS NEVER TOUCHED!]"

uncurry3 : (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3 f = \(x,y,z) -> f x y z

ppTopBindingInfo : H.TopBindingInfo -> PP
ppTopBindingInfo bi = ppBinding (VarTop bi, bi.topBindingRHS)

ppTopBinding : H.TopBinding -> PP
ppTopBinding b = case b of
    H.NonRecTopBinding bi -> ppTopBindingInfo bi
    H.RecTopBinding bis -> 
        ppSeq [ emitText "Rec {"
              , newline
              , indented <| ppSepped "\n\n" (List.map ppTopBindingInfo bis)
              , emitText "}"
              ]

ppBinding_binder : (H.Binder, H.Expr) -> PP
ppBinding_binder (b,e) = ppBinding (VarBinder b, e)

ppBinding : (Var, H.Expr) -> PP
ppBinding (var, e) = 
    let (fe, bs) = H.leadingLambdas e
        in ppSeq [ if H.varIsTopLevel var
                   then ppSeq [ ppVar var
                              , emitText " :: "
                              , ppType (H.varType var)
                              , newline
                              ]
                   else emitText ""
                 , ppVar var
                 , emitText " "
                 , ppSepped " " (List.map ppBinder bs)
                 , emitText (if List.isEmpty bs then "" else " ")
                 , emitText "= ", ppExpr fe
                 ]

ppUnique : H.Unique -> PP
ppUnique (H.Unique _ i) = emitText (String.fromInt i)

ppExpr : H.Expr -> PP
ppExpr expr = case expr of
    H.EVar (H.BinderId _ getBinder) -> ppBinderT (getBinder ())
    H.EVarGlobal name -> ppVar (VarExternal name)
    H.ELit lit -> ppLit lit
    H.ETyLam b e -> ppExpr (H.ELam b e)
    H.EApp f a -> ppSeq [ppExpr f, emitText " ", parensExpr a]
    H.ELam b e -> ppSeq [emitText "\\", ppBinder b, emitText " -> ", indented (ppExpr e)]
    H.ELet bs e ->  ppSeq [ emitKeyword "let "
                          , indented <| ppLines (List.map ppBinding_binder bs)
                          , newline
                          , emitKeyword " in ", ppExpr e
                          ]
    H.ECase e b alts -> ppCase e b alts
    H.EType t -> ppSeq [emitSpan "o" "@", parensType t]
    _ -> emitText "[Expr TODO]"

ppCase : H.Expr -> H.Binder -> List H.Alt -> PP
ppCase e b alts = ppSeq [ emitKeyword "case "
                        , ppExpr e
                        , emitKeyword " of {"
                        , indented <| 
                            ppSeq (List.map (\alt -> ppSeq [ppAlt b alt, newline]) (List.reverse alts))
                        , emitText "}"
                        ]

ppAlt : H.Binder -> H.Alt -> PP
ppAlt b alt = ppSeq [ ppAltCon alt.altCon
                  , ppWhen (not (List.isEmpty alt.altBinders)) (emitText " ")
                  , ppSepped " " (List.map ppBinder (if H.isDefaultAlt alt then [b] else alt.altBinders))
                  , emitText " -> "
                  , ppExpr alt.altRHS
                  ]

ppAltCon : H.AltCon -> PP
ppAltCon con = case con of
    H.AltDataCon s -> if H.isConstructorName s then emitKeyword s else emitText s
    H.AltLit l -> ppLit l
    H.AltDefault -> emitText ""

ppType : H.Type -> PP
ppType type_ = case type_ of
    H.VarTy (H.BinderId _ getBinder) -> case getBinder () of
        H.Found x -> ppBinder x
        H.NotFound -> emitText "[UKNOWN TYPEVAR]"
        H.Untouched -> emitText "[TYPEVAR NEVER TRAVERSED]"
    H.FunTy x y -> ppSeq [parensType x, emitText " -> ", ppType y]
    H.TyConApp (H.TyCon con _) ts -> 
        case ts of
            [] -> emitText con
            _ -> let tsStr = ppSeq (List.intersperse (emitText " ") (List.map ppType ts))
                 in case con of
                    "[]" -> ppSeq [emitText "[", tsStr, emitText "]"]
                    _    -> ppSeq [emitText (con ++ " "), tsStr]
    H.AppTy x y -> ppSeq [ppType x, emitText " ", parensType y]
    H.ForAllTy b t -> 
        let (ft, bs) = H.leadingForalls t
            bndrsStr = ppSepped " " (List.map ppBinder (b::bs))
        in ppSeq [emitText "forall ", bndrsStr, emitText ". ", ppType ft]
    H.LitTy -> emitText "[LitTy]"
    H.CoercionTy -> emitText "[CoercionTy]"
