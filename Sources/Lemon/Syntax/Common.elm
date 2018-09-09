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
  , atom_combine
  , atom_foldl
  , atom_map
  )

--XXX: Deriving Functor for Statement and Atom would be awesome here.

import Lemon.Name exposing (Name)
import List.Extra as List
import Result.Extra as Result



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


atom_map : (a -> b) -> Atom a -> Atom b
atom_map func atom =
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


atom_foldl : (e -> a -> a) -> a -> Atom e -> a
atom_foldl func accum atom =
  case atom of
    Some expr ->
      func expr accum
    List exprs ->
      List.foldl func accum exprs
    Record fields ->
      List.foldl func accum <| List.map Tuple.second fields
    other -> accum


atom_combine : Atom (Result x e) -> Result x (Atom e)
atom_combine atom =
  case atom of
    Basic basic ->
      Ok <| Basic basic
    Variable name ->
      Ok <| Variable name
    None -> Ok <| None
    Some expr ->
      Result.map Some expr
    List exprs ->
      Result.map List <| Result.combine exprs
    Record fields ->
      let
        ( names, values ) = List.unzip fields
      in
      Result.map (Record << List.zip names) <| Result.combine values


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
