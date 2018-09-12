module Lemon.Syntax.Common.Atom exposing
  ( Atom(..)
  , Basic(..)
  , combine
  , foldl
  , map
  )

import Helpers.List as List
import Lemon.Name exposing (..)
import List.Extra as List
import Result.Extra as Result


type Atom e
  = Basic Basic
  | Variable Name
  | None
  | Some e
  | Cons e e
  | End
  | Record (Fields e)


type Basic
  = Bool Bool
  | Int Int
  | Float Float
  | String String


map : (a -> b) -> Atom a -> Atom b
map func atom =
  case atom of
    Basic basic ->
      Basic basic
    Variable name ->
      Variable name
    None -> None
    Some expr ->
      Some (func expr)
    End -> End
    Cons left right ->
      Cons (func left) (func right)
    Record fields ->
      Record <| List.map (Tuple.mapSecond func) fields


foldl : (e -> a -> a) -> a -> Atom e -> a
foldl func accum atom =
  case atom of
    Some expr ->
      func expr accum
    Cons left right ->
      List.foldl func accum [ left, right ]
    Record fields ->
      List.foldl func accum <| List.map Tuple.second fields
    other -> accum


combine : Atom (Result x e) -> Result x (Atom e)
combine atom =
  case atom of
    Basic basic ->
      Ok <| Basic basic
    Variable name ->
      Ok <| Variable name
    None -> Ok <| None
    Some expr ->
      Result.map Some expr
    End -> Ok <| End
    Cons left right ->
      Result.map2 Cons left right
    Record fields ->
      let
        ( names, values ) = List.unzip fields
      in
      Result.map (Record << List.zip names) <| List.combine values
