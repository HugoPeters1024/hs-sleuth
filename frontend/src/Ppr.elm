module Ppr exposing (..)

import Types exposing (..)
import Generated.Types exposing (..)
import HsCore.Helpers exposing (..)

import Pretty exposing (..)
import Pretty.Renderer exposing (..)

type Tag 
    = TagVar Var
    | TagLitString
    | TagLitNumber
    | TagKeyword
    | TagComment
    | TagOperator
    | TagModule

type alias PP = Doc Tag

keyword : String -> PP
keyword t = taggedString t TagKeyword

doubleline : PP
doubleline = append line line

combine : List PP -> PP
combine = Pretty.fold a

pprPhase : String -> Phase -> PP
pprPhase modname phase 
    =  keyword "module"
    |> a space
    |> a (string modname)
    |> a space
    |> a (keyword "where")
    |> a doubleline
    |> a (join doubleline (List.map pprTopBinding phase.phaseTopBindings))
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

pprTopBinding : TopBinding -> PP
pprTopBinding topb = case topb of
    NonRecTopBinding tinfo -> pprTopBindingInfo tinfo
    RecTopBinding tinfos -> 
        string "Rec {"
        |> a line
        |> a (indent 4 (join doubleline (List.map pprTopBindingInfo tinfos)))
        |> a line
        |> a (string "}")

pprTopBindingInfo : TopBindingInfo -> PP
pprTopBindingInfo tb = combine 
    [ words [pprVar (VarTop tb), string "::", pprType (binderType tb.topBindingBinder)]
    , line
    , pprBinding_ (VarTop tb, tb.topBindingRHS)
    ]

pprBinding_ : (Var, Expr) -> PP
pprBinding_ (var, expr) = 
    let (fexpr, bs) = leadingLambdas expr
    in nest 4 <| combine
        [ words (List.map pprVar (var :: List.map VarBinder bs)) 
        , string " = "
        , combine
            [ pprExpr fexpr
            ]
        ]

pprBinding : (Binder, Expr) -> PP
pprBinding (bndr, expr) = pprBinding_ (VarBinder bndr, expr)


pprBinder : Binder -> PP
pprBinder = pprVar << VarBinder

pprExprParens : Expr -> PP
pprExprParens expr = if exprIsAtom expr then pprExpr expr else parens (pprExpr expr)


pprExpr : Expr -> PP
pprExpr expr = case expr of
    EVar varid -> pprBinderThunk (varid.binderIdThunk ())
    EVarGlobal ename -> pprVar (VarExternal ename)
    ELit lit -> pprLit lit
    EApp f a -> hang 2 (combine [pprExpr f, line, pprExprParens a])
    ETyLam b e -> pprExpr (ELam b e)
    ELam b e -> 
        let (fe, bs) = leadingLambdas e
        in combine
            [ string "\\"
            , join space (List.map pprBinder (b::bs))
            , string " ->"
            , softline
            , pprExpr fe
            ]
    ELet bs e -> combine
        [ combine 
            [ tightline
            , keyword "let "
            , align <| join line (List.map pprBinding bs)
            ]
        , line
        , keyword "in "
        , pprExpr e
        ]

    ECase e b alts -> combine
        [ keyword "case"
        , space
        , pprExpr e
        , space
        , keyword "of"
        , line
        , align <| combine
            [ string "{"
            , indent 1 <| join line (List.map (pprAlt b) (List.reverse alts))
            , line
            , string "}"
            ]
        ]
    ETick _ _ -> string "tick"
    EType t -> 
        taggedString "@" TagOperator
        |> a (pprType t)
    ECoercion -> string "coercion"
    EMarkDiff e -> pprExpr e

pprAlt : Binder -> Alt -> PP
pprAlt b alt = nest 4 <| combine
    [ pprAltCon b alt.altCon
    , if List.isEmpty alt.altBinders
      then empty
      else space
    , words (List.map pprBinder alt.altBinders)
    , space
    , string "->"
    , softline
    , pprExpr alt.altRHS
    ]

pprAltCon : Binder -> AltCon -> PP
pprAltCon b con = case con of
    AltDataCon s -> if isConstructorName s then keyword s else string s
    AltLit l -> pprLit l
    AltDefault -> pprBinder b

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


pprBinderThunk : BinderThunk -> PP
pprBinderThunk thunk = case thunk of
    Found b -> pprBinder b
    NotFound -> string "[Binder Not Found]"
    Untouched -> string "[Binder Not Touched]"


pprVar : Var -> PP
pprVar var = 
    let cname = varName var
        tag = TagVar var
    in case var of
        VarBinder bndr ->
            if HsCore.Helpers.binderIsUnused bndr
            then taggedString "_" tag
            else taggedString cname tag
        VarExternal (ExternalName ext) -> combine [Pretty.taggedString (ext.externalModuleName ++ ".") TagModule, Pretty.taggedString cname tag]
        _ -> taggedString cname tag


pprType : Type -> PP
pprType type_ = case type_ of
    VarTy var -> case var.binderIdThunk () of
        Found x -> pprBinder x
        NotFound -> string "[UNKNOWN TYPEVAR]"
        Untouched -> string "[TYPEVAR UNTRAVERSED]"
    FunTy x e -> combine [pprTypeParens x, string " -> ", pprType e]
    TyConApp (TyCon con _) ts ->
        case ts of
            [] -> string con
            _ -> let tsStr = join space (List.map pprType ts)
                 in case con of
                    "[]" -> brackets tsStr
                    _    -> combine [string con, space, tsStr]
    AppTy f a -> combine [pprType f, space, pprTypeParens a]
    ForAllTy b t ->
        let (ft, bs) = leadingForalls t
            bndrs = join space (List.map pprBinder (b::bs))
        in combine [string "forall ", bndrs, string ". ", pprType ft]
    LitTy -> string "[LIT TYPE??]"
    CoercionTy -> string "[COERCION TYPE??]" 

pprTypeParens : Type -> PP
pprTypeParens type_ = case type_ of
    VarTy t -> pprType (VarTy t)
    TyConApp con xs -> pprType (TyConApp con xs)
    _ -> parens (pprType type_)

