module PprGHC exposing (..)

import Types exposing (..)
import Generated.Types exposing (..)
import HsCore.Helpers exposing (..)
import ElmHelpers as EH

import Pretty exposing (..)
import Pretty.Renderer exposing (..)
import Dict

type BindingSite
  = LambdaBind
  | CaseBind
  | CasePatBind
  | LetBind

type Tag 
    = TagVar Var
    | TagLitString
    | TagLitNumber
    | TagKeyword
    | TagComment
    | TagOperator
    | TagModule

type alias PP = Doc Tag

type alias Annotation = Expr -> PP

noAnn : Annotation
noAnn _ = empty

noParens : PP -> PP
noParens = identity

type alias Env =
  { opt_SuppressTypeSignatures : Bool
  , opt_SuppressTypeApplications : Bool
  }

default_env : Env
default_env =
  { opt_SuppressTypeSignatures = False
  , opt_SuppressTypeApplications = False
  }

keyword : String -> PP
keyword t = taggedString t TagKeyword

doubleline : PP
doubleline = append line line

combine : List PP -> PP
combine = Pretty.fold a

vcat : List PP -> PP
vcat = Pretty.fold (a << a line)

sep : List PP -> PP
sep = Pretty.fold (a << a space)

hang : PP -> Int -> PP -> PP
hang d1 n d2 = sep [d1, nest n d2]

punctuate : PP -> List PP -> List PP
punctuate p list = case list of
  [] -> []
  d_::ds -> 
    let go d llist = case llist of
          [] -> [d]
          (e::es) -> combine [d,p] :: go e es
    in go d_ ds

pprWithCommas : (a -> PP) -> List a -> PP
pprWithCommas pp xs = sep (punctuate (char ',') (List.map pp xs))


pprPhase : Env -> String -> Phase -> PP
pprPhase env modname phase 
    =  keyword "module"
    |> a space
    |> a (string modname)
    |> a space
    |> a (keyword "where")
--    |> a line
--    |> a (pprFiredRules phase)
    |> a doubleline
    |> a (join doubleline (List.map (pprTopBind env noAnn) phase.phaseTopBindings))
    |> a doubleline

pprTopBind : Env -> Annotation -> TopBinding -> PP
pprTopBind env ann tb = case tb of
  NonRecTopBinding ti -> ppr_binding env ann (topBindToBinding ti)
  RecTopBinding []  -> string "Rec { }"
  RecTopBinding (ti::tis) ->
    vcat [ string "Rec {"
         , ppr_binding env ann (ti.topBindingBinder, ti.topBindingRHS)
         , vcat <| List.map (ppr_binding env ann << topBindToBinding) tis
         , string "end Rec }"
         ]


ppr_binding : Env -> Annotation -> (Binder, Expr) -> PP
ppr_binding env ann (val_bdr, expr) = 
  let pp_bind = case bndrIsJoin_maybe val_bdr of 
                  Nothing -> pp_normal_bind
                  Just ar -> pp_join_bind ar

      pp_normal_bind = hang (pprVar val_bdr) 2 (combine [Pretty.char '=', space, pprCoreExpr env expr])
      pp_join_bind join_arity = string "pp_join_bind"
  in
  vcat [ ann expr
       , if env.opt_SuppressTypeSignatures then empty else pprBndr LetBind val_bdr
       , pp_bind
       ]

pprVar : Binder -> PP
pprVar b = string (binderName b)

pprCoreExpr : Env -> Expr -> PP
pprCoreExpr = ppr_expr noParens

pprParendExpr : Env -> Expr -> PP
pprParendExpr = ppr_expr parens

ppr_expr : (PP -> PP) -> Env -> Expr -> PP
ppr_expr add_par env expr = case expr of
  EVar id -> 
    case id.binderIdThunk of
      Found name ->
          if isJoinId name 
          then add_par (combine [(string "jump"), space, ppr_name name])
          else ppr_name name
      NotFound -> string "[BINDER OUT OF SCOPE]"
      Untouched -> string "[AST NO RECONSTRUCTED]"
  EVarGlobal g -> string "Module.variable"
  EType ty -> string "type"
  ECoercion -> string "COERCION"
  ELit lit -> pprLit lit
  ELam _ _ ->
    let (bs, body) = collectBinders expr
    in add_par <| hang (combine [string "\\", sep (List.map (pprBndr LambdaBind) bs), string "->"]) 2 (pprCoreExpr env body)
  EApp _ _ ->
    let (fun, args) = collectArgs expr
        pp_args = sep (List.map (pprArg env) args)
        val_args = EH.dropWhile isTypeArg args
        pp_tup_args = pprWithCommas (pprCoreExpr env) val_args
        real_args = if env.opt_SuppressTypeApplications then val_args else args
        parens = if List.isEmpty real_args then identity else add_par
    in parens (hang (pprParendExpr env fun) 2 pp_args)
  _ -> string "ppr_expr"

ppr_name : Binder -> PP
ppr_name b = string (binderName b)

pprBndr : BindingSite -> Binder -> PP
pprBndr _ = ppr_name

pprArg : Env -> Expr -> PP
pprArg env expr = case expr of
  EType ty -> if env.opt_SuppressTypeApplications then empty else combine [string "@", ppr_type ty]
  _       -> pprParendExpr env expr

ppr_type : Type -> PP
ppr_type _ = string "type"

topBindToBinding : TopBindingInfo -> (Binder, Expr)
topBindToBinding ti = (ti.topBindingBinder, ti.topBindingRHS)


bndrIsJoin_maybe : Binder -> Maybe Int
bndrIsJoin_maybe binder = case binder of
  Binder b -> case b.binderIdDetails of
                JoinId arity -> Just arity.joinIdArity
                _            -> Nothing
  _        -> Nothing


isJoinId : Binder -> Bool
isJoinId id = case bndrIsJoin_maybe id of
  Nothing -> False
  Just _  -> True

isTypeArg : Expr -> Bool
isTypeArg expr = case expr of
  EType _ -> True
  _       -> False

collectBinders : Expr -> (List Binder, Expr)
collectBinders =
  let go bs expr = case expr of
        ELam b e -> go (b::bs) e
        _        -> (List.reverse bs, expr)
  in go []

collectArgs : Expr -> (Expr, List Expr)
collectArgs e =
  let go expr ass = case expr of
        EApp f a -> go f (a::ass)
        _       -> (expr, ass)
  in go e []

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
