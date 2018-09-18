module Lemon.Syntax.Textual exposing
  ( Alternative
  , Atom(..)
  , Branch
  , Declaration(..)
  , Expression(..)
  , Fields
  , Module(..)
  , Parameter
  , Pattern(..)
  , Scope
  , Statement(..)
  , Type(..)
  , empty
  )

import Helpers.Hole exposing (..)
import Lemon.Names exposing (..)
import Lemon.Syntax.Common exposing (..)
import Lemon.Types exposing (BasicType)



-- Data ------------------------------------------------------------------------
-- Modules and declarations --


type Module
  = Module Scope


type alias Scope =
  List Declaration


type Declaration
  = Value Name Type Name (List Pattern) Expression



-- Expressions --


type Expression
  = Atom Atom
  | Lambda (List Parameter) Expression
  | Call Expression (List Expression)
  | Let Scope Expression
  | Case Expression (List Alternative)
  | If Expression Expression Expression
  | Sequence (List Statement)


type alias Alternative =
  ( Pattern, Expression )



-- Atoms --


type Atom
  = ABasic Basic
  | AVariable Name
  | AJust Expression
  | ANothing
  | ACons Expression Expression
  | ANil
  | ARecord (Fields Expression)


type alias Fields a =
  List ( Name, a )



-- Statements --


type Statement
  = SLet Pattern Expression
  | SBind Pattern Expression
  | SIgnore Expression
  | SPar (List (List Statement))
  | SWhen (List Branch)
  | SOn (List ( Name, Branch ))
  | SDone


type alias Branch =
  ( Expression, List Statement )



-- Patterns --


type Pattern
  = PBasic Basic
  | PVariable Name
  | PJust Pattern
  | PNothing
  | PCons Pattern Pattern
  | PNil
  | PRecord (Fields Pattern)
  | PIgnore


type alias Parameter =
  ( Pattern, Type )



-- Patterns --


type Type
  = TBasic BasicType
  | TVariable Name
  | TMaybe Type
  | TList Type
  | TRecord (Fields Type)
  | TTask Type
  | TArrow Type Type



-- Init ------------------------------------------------------------------------


empty : Scope
empty = []



-- Traversals ------------------------------------------------------------------
{-
   map :
     (Declaration -> Declaration)
     -> (Expression -> Expression)
     -> (Atom -> Atom)
     -> (Statement -> Statement)
     -> (Pattern -> Pattern)
     -> (Type -> Type)
     -> { d : Declaration -> Declaration, e : Expression -> Expression, a : Atom -> Atom, s : Statement -> Statement, p : Pattern -> Pattern, t : Type -> Type }
   map d e a s p t =
     let
       d_ = hole
       e_ = hole
       a_ = hole
       s_ = hole
       p_ = hole
       t_ = hole
     in
     { d = d_, e = e_, a = a_, s = s_, p = p_, t = t_ }
-}


foldl :
  (r -> r -> r)
  -> (Declaration -> r)
  -> (Expression -> r)
  -> (Atom -> r)
  -> (Statement -> r)
  -> (Pattern -> r)
  -> (Type -> r)
  ->
    { d : Declaration -> r
    , e : Expression -> r
    , a : Atom -> r
    , s : Statement -> r
    , p : Pattern -> r
    , t : Type -> r
    }
foldl op d e a s p t =
  let
    --
    -- Helpers
    fold = List.foldl op
    combine f g ( x, y ) =
      op (f x) (g y)
    --
    -- Traversals
    d_ decl =
      fold (d decl) <|
        case decl of
          Value name1 tipe name2 patts expr -> [ t tipe ] ++ List.map p patts ++ [ e expr ]
    e_ expr =
      fold (e expr) <|
        case expr of
          Atom atom -> [ a atom ]
          Lambda parms body -> List.map (combine p t) parms ++ [ e body ]
          Call func exprs -> [ e func ] ++ List.map e exprs
          Let scop body -> List.map d scop ++ [ e body ]
          Case test alts -> [ e test ] ++ List.map (combine p e) alts
          If test pos neg -> [ e test, e pos, e neg ]
          Sequence stmts -> List.map s stmts
    a_ atom =
      fold (a atom) <|
        case atom of
          ABasic basc -> []
          AVariable name -> []
          AJust expr -> [ e expr ]
          ANothing -> []
          ACons head tail -> [ e head, e tail ]
          ANil -> []
          ARecord fields -> List.map (\( _, expr ) -> e expr) fields
    s_ stmt =
      fold (s stmt) <|
        case stmt of
          SLet patt expr -> [ p patt, e expr ]
          SBind patt expr -> [ p patt, e expr ]
          SIgnore expr -> [ e expr ]
          SPar stmts -> List.concat <| List.map (List.map s) stmts
          SWhen brncs -> List.map (\( expr, stmts ) -> fold (e expr) (List.map s stmts)) brncs
          SOn brncs -> List.map (\( _, ( expr, stmts ) ) -> fold (e expr) (List.map s stmts)) brncs
          SDone -> []
    p_ patt =
      fold (p patt) <|
        case patt of
          PBasic basic -> []
          PVariable name -> []
          PJust inner -> [ p inner ]
          PNothing -> []
          PCons head tail -> [ p head, p tail ]
          PNil -> []
          PRecord fields -> List.map (\( _, inner ) -> p inner) fields
          PIgnore -> []
    t_ tipe =
      fold (t tipe) <|
        case tipe of
          TBasic basic -> []
          TVariable name -> []
          TMaybe inner -> [ t inner ]
          TList inner -> [ t inner ]
          TRecord fields -> List.map (\( _, inner ) -> t inner) fields
          TTask inner -> [ t inner ]
          TArrow left right -> [ t left, t right ]
  in
  { d = d_, e = e_, a = a_, s = s_, p = p_, t = t_ }



{-
   traverse :
     (Declaration -> Result x d)
     -> (Expression -> Result x e)
     -> (Atom -> Result x a)
     -> (Statement -> Result x s)
     -> (Pattern -> Result x p)
     -> (Type -> Result x t)
     ->
       { d : Declaration -> Result x d
       , e : Expression -> Result x e
       , a : Atom -> Result x a
       , s : Statement -> Result x s
       , p : Pattern -> Result x p
       , t : Type -> Result x t
       }
   traverse d e a s p t =
     let
       d_ decl =
         hole
       e_ expr =
         hole
       a_ atom =
         hole
       s_ stmt =
         hole
       p_ patt =
         hole
       t_ tipe =
         hole
     in
     { d = d_, e = e_, a = a_, s = s_, p = p_, t = t_ }
-}
