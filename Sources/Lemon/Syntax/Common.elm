module Lemon.Syntax.Common exposing
  ( Alternative
  , Atom(..)
  , Basic(..)
  , BasicType(..)
  , Fields
  , Parameter
  , Pattern(..)
  , Statement(..)
  , Type(..)
  )

--XXX: Deriving Functor for Statement and Atom would be awesome here.

import Lemon.Name exposing (Name)


type Hole
  = Hole



-- Statements ------------------------------------------------------------------


type Statement e
  = Set Pattern e
  | Bind Pattern e
  | Do e
  | Par (List (List (Statement e)))
  | On (List ( Name, e, List (Statement e) ))
  | When (List ( e, List (Statement e) ))
  | Done



-- Atoms -----------------------------------------------------------------------


type Atom e
  = Basic Basic
  | Variable Name
  | None
  | Some e
  | List (List e)
  | Record (Fields e)


mapAtom : (a -> b) -> Atom a -> Atom b
mapAtom func atom =
  case atom of
    Basic basic ->
      Basic basic
    Variable name ->
      Variable name
    None -> None
    Some expr ->
      Some <| func expr
    List exprs ->
      List <| List.map func exprs
    Record fields ->
      Record <| List.map (Tuple.mapSecond func) fields


type alias Fields a =
  List ( Name, a )


type Basic
  = Bool Bool
  | Int Int
  | Float Float
  | String String



-- Patterns --------------------------------------------------------------------


type Pattern
  = PBasic Basic
  | PVariable Name
  | PSome Pattern
  | PNone
  | PCons Pattern Pattern
  | PNil
  | PRecord (Fields Pattern)
  | PIgnore


type alias Parameter =
  ( Pattern, Type )


type alias Alternative e =
  ( Pattern, e )



-- Types -----------------------------------------------------------------------


type Type
  = TBasic BasicType
  | TVariable Name
  | TOption Type
  | TList Type
  | TRecord (Fields Type)
  | TTask Type
  | TArrow Type Type


type BasicType
  = TBool
  | TInt
  | TFloat
  | TString
