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
  = ABasic Basic
  | AVariable Name
  | AJust e
  | ANothing
  | ACons e e
  | AEnd
  | ARecord (Fields e)


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
    AJust expr ->
      AJust (func expr)
    ANothing -> ANothing
    ACons left right ->
      ACons (func left) (func right)
    AEnd -> AEnd
    ARecord fields ->
      ARecord <| List.map (Tuple.mapSecond func) fields


foldl : (e -> a -> a) -> a -> Atom e -> a
foldl func accum atom =
  case atom of
    ABasic _ ->
      accum
    AVariable _ ->
      accum
    AJust expr ->
      func expr accum
    ANothing -> accum
    ACons left right ->
      List.foldl func accum [ left, right ]
    AEnd -> accum
    ARecord fields ->
      List.foldl func accum <| List.map Tuple.second fields


combine : Atom (Result x e) -> Result x (Atom e)
combine atom =
  case atom of
    ABasic basic ->
      Ok <| ABasic basic
    AVariable name ->
      Ok <| AVariable name
    AJust expr ->
      Result.map AJust expr
    ANothing -> Ok <| ANothing
    ACons left right ->
      Result.map2 ACons left right
    AEnd -> Ok <| AEnd
    ARecord fields ->
      let
        ( names, values ) = List.unzip fields
      in
      Result.map (ARecord << List.zip names) <| List.combine values
