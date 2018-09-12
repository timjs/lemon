module Lemon.Syntax.Common.Atom exposing
  ( Atom(..)
  , Basic(..)
  , combine
  , cons
  , end
  , foldl
  , just
  , map
  , nothing
  )

import Helpers.List as List
import Lemon.Name exposing (..)
import List.Extra as List
import Result.Extra as Result


type Atom e
  = ABasic Basic
  | AVariable Name
  | AConstructor Name (List e)
  | ARecord (Fields e)


just : e -> Atom e
just x =
  AConstructor "Just" [ x ]


nothing : Atom e
nothing = AConstructor "Nothing" []


cons : e -> e -> Atom e
cons head tail =
  AConstructor "Cons" [ head, tail ]


end : Atom e
end = AConstructor "End" []


type Basic
  = Bool Bool
  | Int Int
  | Float Float
  | String String


map : (a -> b) -> Atom a -> Atom b
map func atom =
  case atom of
    ABasic basic ->
      ABasic basic
    AVariable name ->
      AVariable name
    AConstructor name exprs ->
      AConstructor name (List.map func exprs)
    ARecord fields ->
      ARecord <| List.map (Tuple.mapSecond func) fields


foldl : (e -> a -> a) -> a -> Atom e -> a
foldl func accum atom =
  case atom of
    ABasic _ ->
      accum
    AVariable _ ->
      accum
    AConstructor _ exprs ->
      List.foldl func accum exprs
    ARecord fields ->
      List.foldl func accum <| List.map Tuple.second fields


combine : Atom (Result x e) -> Result x (Atom e)
combine atom =
  case atom of
    ABasic basic ->
      Ok <| ABasic basic
    AVariable name ->
      Ok <| AVariable name
    AConstructor name exprs ->
      Result.map (AConstructor name) <| List.combine exprs
    ARecord fields ->
      let
        ( names, values ) = List.unzip fields
      in
      Result.map (ARecord << List.zip names) <| List.combine values
