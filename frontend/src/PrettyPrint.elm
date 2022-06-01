module PrettyPrint exposing (..)

import Types exposing (..)
import Generated.Types exposing (..)
import HsCore.Helpers exposing (..)
import State exposing (State)
import State as S

import Dict exposing (Dict)
import Char
import Either exposing (Either(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Reader exposing (Reader(..))

type alias PPEnv = { selectedVar : Maybe Var
                   , onClickBinder : Var -> Msg
                   , renderVarName : Var -> String
                   , renderVarAttributes : Var -> List (Attribute Msg)
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
    { selectedVar = Nothing
    , onClickBinder = MsgCodeMsg tid << CodeMsgSelectVar
    , renderVarName = varName False
    , renderVarAttributes = \_ -> []
    }

withFullNameBinder : PPEnv -> PPEnv
withFullNameBinder env = { env | renderVarName = varName True }

varHighlightClass : PPEnv -> Var -> String
varHighlightClass env o = case env.selectedVar of
    Nothing -> ""
    Just v -> 
        if varToInt v == varToInt o
        then ( if varPhaseId v == varPhaseId o
               then "highlight-exact"  
               else "highlight-approx"
             )
        else ""

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

parensExpr : Expr -> PP
parensExpr expr = case expr of
    EVar b -> ppExpr (EVar b)
    EVarGlobal b -> ppExpr (EVarGlobal b)
    ELit l -> ppExpr (ELit l)
    EType t -> ppExpr (EType t)
    _        -> parens (ppExpr expr)

parensType : Type -> PP
parensType type_ = case type_ of
    VarTy t -> ppType (VarTy t)
    TyConApp con xs -> ppType (TyConApp con xs)
    _         -> parens (ppType type_)

ppLit : Lit -> PP
ppLit lit  = case lit of
    MachChar c -> emitSpan "s" (String.fromList ['\'', c, '\''])
    MachStr s  -> emitSpan "s" ("" ++ s ++ "")
    MachNullAddr -> emitKeyword "NullAddr#"
    MachInt i  -> emitSpan "m" i
    MachInt64 i -> emitSpan "m" i
    MachWord i -> emitSpan "m" i
    MachWord64 i -> emitSpan "m" i
    MachFloat f -> emitSpan "m" f
    MachDouble d -> emitSpan "m" d
    MachLabel l -> emitText l
    LitInteger i  -> emitSpan "m" i
    LitNatural n -> emitSpan "m" n
    LitRubbish -> emitText "[LitRubbish]"

getVarClasses : PPEnv -> Var -> List (Attribute msg)
getVarClasses env var = List.concat 
    [  [class (varHighlightClass env var)]
    ,  if varIsConstructor var then [class "k"] else []
    ,  if varIsTopLevel var then [class "nf"] else []
    ]

ppVar : Var -> PP
ppVar var = Reader.ask |> Reader.andThen (\env -> case varExternalLocalBinder var of
    Just b -> ppVar (VarBinder b)
    Nothing -> emit <|
        a [class "no-style"]
          [ span (onClick (env.onClickBinder var)::(getVarClasses env var ++ env.renderVarAttributes var)) [text (env.renderVarName var)]
          ]
    )

ppBinder : Binder -> PP
ppBinder = ppVar << VarBinder

ppBinderT : BinderThunk -> PP
ppBinderT mb = case mb of
    Found b -> ppVar (VarBinder b)
    NotFound -> emitText "[!UKNOWN VARIABLE!]" 
    Untouched -> emitText "[!I WAS NEVER TOUCHED!]"

uncurry3 : (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3 f = \(x,y,z) -> f x y z

ppTopBindingInfo : TopBindingInfo -> PP
ppTopBindingInfo bi = ppBinding (VarTop bi, bi.topBindingRHS)

ppTopBinding : TopBinding -> PP
ppTopBinding b = case b of
    NonRecTopBinding bi -> ppTopBindingInfo bi
    RecTopBinding bis -> 
        ppSeq [ emitText "Rec {"
              , newline
              , indented <| ppSepped "\n\n" (List.map ppTopBindingInfo bis)
              , emitText "}"
              ]

ppBinding_binder : (Binder, Expr) -> PP
ppBinding_binder (b,e) = ppBinding (VarBinder b, e)

ppBinding : (Var, Expr) -> PP
ppBinding (var, e) = 
    let (fe, bs) = leadingLambdas e
        in ppSeq [ if varIsTopLevel var
                   then ppSeq [ ppVar var
                              , emitText " :: "
                              , ppType (varType var)
                              , newline
                              ]
                   else emitText ""
                 , ppVar var
                 , emitText " "
                 , ppSepped " " (List.map ppBinder bs)
                 , emitText (if List.isEmpty bs then "" else " ")
                 , emitText "= "
                 , if exprIsSmall e
                   then ppExpr fe
                   else indented (ppExpr fe)
                 ]

ppUnique : Unique -> PP
ppUnique (Unique _ i) = emitText (String.fromInt i)

ppExpr : Expr -> PP
ppExpr expr = case expr of
    EVar (BinderId _ getBinder) -> ppBinderT (getBinder ())
    EVarGlobal name -> ppVar (VarExternal name)
    ELit lit -> ppLit lit
    ETyLam b e -> ppExpr (ELam b e)
    EApp f a -> 
        if exprIsSmall (EApp f a)
        then ppSeq [ppExpr f, emitText " ", parensExpr a]
        else ppSeq [ppExpr f, newline, parensExpr a]
    ELam b e -> ppSeq [emitText "\\", ppBinder b, emitText " -> ", indented (ppExpr e)]
    ELet bs e ->  ppSeq [ emitKeyword "let "
                          , indented <| ppLines (List.map ppBinding_binder bs)
                          , newline
                          , emitKeyword " in ", ppExpr e
                          ]
    ECase e b alts -> ppCase e b alts
    EType t -> ppSeq [emitSpan "o" "@", parensType t]
    ECoercion -> emitText "[Coercion]"
    ETick _ e -> ppExpr e
    EMarkDiff e -> Reader.ask |> Reader.andThen (\env -> emit (span [class "diff"] (prettyPrint env (ppExpr e))))

ppCase : Expr -> Binder -> List Alt -> PP
ppCase e b alts = ppSeq [ emitKeyword "case "
                        , if exprIsSmall e
                          then ppExpr e
                          else indented <| ppExpr e
                        , emitKeyword " of {"
                        , indented <| 
                            ppSeq (List.map (\alt -> ppSeq [ppAlt b alt, newline]) (List.reverse alts))
                        , emitText "}"
                        ]

ppAlt : Binder -> Alt -> PP
ppAlt b alt = ppSeq [ ppAltCon alt.altCon
                  , ppWhen (not (List.isEmpty alt.altBinders)) (emitText " ")
                  , ppSepped " " (List.map ppBinder (if isDefaultAlt alt then [b] else alt.altBinders))
                  , emitText " -> "
                  , ppExpr alt.altRHS
                  ]

ppAltCon : AltCon -> PP
ppAltCon con = case con of
    AltDataCon s -> if isConstructorName s then emitKeyword s else emitText s
    AltLit l -> ppLit l
    AltDefault -> emitText ""

ppType : Type -> PP
ppType type_ = case type_ of
    VarTy (BinderId _ getBinder) -> case getBinder () of
        Found x -> ppBinder x
        NotFound -> emitText "[UKNOWN TYPEVAR]"
        Untouched -> emitText "[TYPEVAR NEVER TRAVERSED]"
    FunTy x y -> ppSeq [parensType x, emitText " -> ", ppType y]
    TyConApp (TyCon con _) ts -> 
        case ts of
            [] -> emitText con
            _ -> let tsStr = ppSeq (List.intersperse (emitText " ") (List.map ppType ts))
                 in case con of
                    "[]" -> ppSeq [emitText "[", tsStr, emitText "]"]
                    _    -> ppSeq [emitText (con ++ " "), tsStr]
    AppTy x y -> ppSeq [ppType x, emitText " ", parensType y]
    ForAllTy b t -> 
        let (ft, bs) = leadingForalls t
            bndrsStr = ppSepped " " (List.map ppBinder (b::bs))
        in ppSeq [emitText "forall ", bndrsStr, emitText ". ", ppType ft]
    LitTy -> emitText "[LitTy]"
    CoercionTy -> emitText "[CoercionTy]"
