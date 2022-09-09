module Ppr exposing (..)

import Types exposing (..)
import Generated.Types exposing (..)
import HsCore.Helpers exposing (..)

import Pretty exposing (..)
import Pretty.Renderer exposing (..)
import Dict exposing (Dict)

type Tag 
    = TagVar Var
    | TagLitString
    | TagLitNumber
    | TagKeyword
    | TagComment
    | TagOperator
    | TagModule

type alias PP = Doc Tag

renderVarName : CodeViewOptions -> Var -> String
renderVarName env var = 
    let postfix : String -> String
        postfix i = if env.hideDisambiguation then i else i ++ "_" ++ HsCore.Helpers.varGHCUnique var

        renamed : String -> String
        renamed i = case Dict.get (varToInt var) env.varRenames of
          Just o -> o
          Nothing -> i

        disabled : String -> String
        disabled i = case var of
          VarBinder bndr ->
            if HsCore.Helpers.binderIsUnused bndr then "_" else i
          _ -> i

        prefix : String -> String
        prefix i = case var of
          VarExternal (ExternalName e) ->
            if env.hideModules then i else e.externalModuleName ++ "." ++ i
          _ -> i 

    in
      varName var
      |> renamed
      |> postfix
      |> disabled
      |> prefix

keyword : String -> PP
keyword t = taggedString t TagKeyword

doubleline : PP
doubleline = append line line

combine : List PP -> PP
combine = Pretty.fold a

pprPhase : CodeViewOptions -> String -> Phase -> PP
pprPhase env modname phase 
    =  keyword "module"
    |> a space
    |> a (string modname)
    |> a space
    |> a (keyword "where")
    |> a doubleline
    |> a (join doubleline (List.map (pprTopBinding env) phase.phaseTopBindings))
    |> a doubleline
    |> a (pprFiredRules phase)

pprFiredRules : Phase -> PP
pprFiredRules phase = pprComment <|
    ( String.join "\n    " 
        <| List.concat
            [ ["{-", "RULES FIRED:"]
            , List.map (\r -> r.firedRuleName ++ " (" ++ r.firedRuleModule ++ ")") phase.phaseFiredRules
            ]
    )
    ++ "\n-}"

pprComment : String -> PP
pprComment s = taggedString s TagComment

pprTopBinding : CodeViewOptions -> TopBinding -> PP
pprTopBinding env topb = case topb of
    NonRecTopBinding tinfo -> pprTopBindingInfo env tinfo
    RecTopBinding tinfos -> 
        taggedString ("Rec(" ++ String.fromInt (List.length tinfos) ++ ")") TagOperator
        |> a (string " {")
        |> a line
        |> a (indent 2 (join doubleline (List.map (pprTopBindingInfo env) tinfos)))
        |> a line
        |> a (string "}")

pprTopBindingInfo : CodeViewOptions -> TopBindingInfo -> PP
pprTopBindingInfo env tb = combine 
    [ words [pprVar env (VarTop tb), string "::", pprType env (binderType tb.topBindingBinder)]
    , line
    , pprBinding_ env (VarTop tb, tb.topBindingRHS)
    ]

pprBinding_ : CodeViewOptions -> (Var, Expr) -> PP
pprBinding_ env (var, expr) = 
    let (fexpr, bs) = leadingLambdas expr
    in nest 2 <| combine
        [ words (List.map (pprVar env) (var :: List.map VarBinder bs)) 
        , string " = "
        , combine
            [ pprExpr env fexpr
            ]
        ]

pprBinding : CodeViewOptions -> (Binder, Expr) -> PP
pprBinding env (bndr, expr) = pprBinding_ env (VarBinder bndr, expr)


pprBinder : CodeViewOptions -> Binder -> PP
pprBinder env = pprVar env << VarBinder

pprExprParens : CodeViewOptions -> Expr -> PP
pprExprParens env expr = if exprIsAtom expr then pprExpr env expr else parens (pprExpr env expr)


pprExpr : CodeViewOptions -> Expr -> PP
pprExpr env expr = case expr of
    EVar varid -> pprBinderThunk env (varid.binderIdThunk ())
    EVarGlobal ename -> pprVar env (VarExternal ename)
    ELit lit -> pprLit lit
    EApp f a -> if exprIsAtom a then combine [pprExpr env f, space, pprExpr env a] else hang 2 (combine [pprExpr env f, line, pprExprParens env a])
    ETyLam b e -> pprExpr env (ELam b e)
    ELam b e -> 
        let (fe, bs) = leadingLambdas e
        in combine
            [ string "\\"
            , join space (List.map (pprBinder env) (b::bs))
            , string " ->"
            , softline
            , pprExpr env fe
            ]
    ELet bs e -> combine
        [ combine 
            [ tightline
            , keyword "let "
            , align <| join line (List.map (pprBinding env) bs)
            ]
        , line
        , keyword "in "
        , pprExpr env e
        ]

    ECase e b alts -> combine
        [ keyword "case"
        , space
        , pprExpr env e
        , space
        , keyword "of"
        , line
        , align <| combine
            [ string "{"
            , indent 1 <| join line (List.map (pprAlt env b) (List.reverse alts))
            , line
            , string "}"
            ]
        ]
    ETick _ _ -> string "tick"
    EType t -> 
        taggedString "@" TagOperator
        |> a (pprType env t)
    ECoercion -> string "coercion"
    EMarkDiff e -> pprExpr env e

pprAlt : CodeViewOptions -> Binder -> Alt -> PP
pprAlt env b alt = nest 2 <| combine
    [ pprAltCon env b alt.altCon
    , if List.isEmpty alt.altBinders
      then empty
      else space
    , words (List.map (pprBinder env) alt.altBinders)
    , space
    , string "->"
    , softline
    , pprExpr env alt.altRHS
    ]

pprAltCon : CodeViewOptions -> Binder -> AltCon -> PP
pprAltCon env b con = case con of
    AltDataCon s -> if isConstructorName s then keyword s else string s
    AltLit l -> pprLit l
    AltDefault -> pprBinder env b

pprLit : Lit -> PP
pprLit lit = 
    let litNumber : String -> PP
        litNumber i = taggedString i TagLitNumber
    in case lit of
        MachChar c -> taggedString (String.fromList ['\'', c, '\'']) TagLitString
        MachStr s  -> taggedString s TagLitString
        MachNullAddr -> string "NullAddr#"
        MachInt i  -> litNumber i
        MachInt64 i -> litNumber i
        MachWord i -> litNumber i
        MachWord64 i -> litNumber i
        MachFloat f -> litNumber f
        MachDouble d -> litNumber d
        MachLabel l -> litNumber l
        LitInteger i  -> litNumber i
        LitNatural n -> litNumber n
        LitRubbish -> string "[LitRubbish]"


pprBinderThunk : CodeViewOptions -> BinderThunk -> PP
pprBinderThunk env thunk = case thunk of
    Found b -> pprBinder env b
    NotFound -> string "[Binder Not Found]"
    Untouched -> string "[Binder Not Touched]"


pprVar : CodeViewOptions -> Var -> PP
pprVar env var = Pretty.taggedString (renderVarName env var) (TagVar var)


pprType : CodeViewOptions -> Type -> PP
pprType env type_ = case type_ of
    VarTy var -> case var.binderIdThunk () of
        Found x -> pprBinder env x
        NotFound -> string "[UNKNOWN TYPEVAR]"
        Untouched -> string "[TYPEVAR UNTRAVERSED]"
    FunTy x e -> combine [pprTypeParens env x, string " -> ", pprType env e]
    TyConApp (TyCon con _) ts ->
        case ts of
            [] -> string con
            _ -> let tsStr = join space (List.map (pprType env) ts)
                 in case con of
                    "[]" -> brackets tsStr
                    _    -> combine [string con, space, tsStr]
    AppTy f a -> combine [pprType env f, space, pprTypeParens env a]
    ForAllTy b t ->
        let (ft, bs) = leadingForalls t
            bndrs = join space (List.map (pprBinder env) (b::bs))
        in combine [string "forall ", bndrs, string ". ", pprType env ft]
    LitTy ty -> string (HsCore.Helpers.tyLitToString ty)
    CoercionTy -> string "[COERCION TYPE??]" 

pprTypeParens : CodeViewOptions -> Type -> PP
pprTypeParens env type_ = case type_ of
    VarTy t -> pprType env (VarTy t)
    TyConApp con xs -> pprType env (TyConApp con xs)
    _ -> parens (pprType env type_)

