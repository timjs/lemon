module Lemon.Syntax.Common.Statement exposing
  ( Statement(..)
  , combine
  , map
  )

import Helpers.List as List
import Lemon.Name exposing (Name)
import Lemon.Syntax.Common.Pattern exposing (Pattern)


type Statement e
  = Set Pattern e
  | Bind Pattern e
  | Do e
  | Par (List (List (Statement e)))
  | On (List ( Name, e, List (Statement e) ))
  | When (List ( e, List (Statement e) ))
  | Done


map : (a -> b) -> Statement a -> Statement b
map f stmt =
  let
    mapOn ( name, pred, stmts ) =
      ( name, f pred, List.map (map f) stmts )
    mapWhen ( pred, stmts ) =
      ( f pred, List.map (map f) stmts )
  in
  case stmt of
    Set pat expr ->
      Set pat <| f expr
    Bind pat expr ->
      Bind pat <| f expr
    Do expr ->
      Do <| f expr
    Par exprs ->
      Par <| List.map (List.map (map f)) exprs
    On branches ->
      On <| List.map mapOn branches
    When branches ->
      When <| List.map mapWhen branches
    Done -> Done


combine : Statement (Result x e) -> Result x (Statement e)
combine stmt =
  let
    combineOn ( name, pred, stmts ) =
      let
        fix ( pred_new, stmts_new ) =
          ( name, pred_new, stmts_new )
      in
      Result.map fix <| combineWhen ( pred, stmts )
    combineWhen ( pred, stmts ) =
      Result.map2 Tuple.pair pred (List.combine <| List.map combine stmts)
  in
  case stmt of
    Set pat expr ->
      Result.map (Set pat) <| expr
    Bind pat expr ->
      Result.map (Bind pat) <| expr
    Do expr ->
      Result.map Do <| expr
    Par branches ->
      Result.map Par <| List.combine (List.map List.combine (List.map (List.map combine) branches))
    On branches ->
      Result.map On <| List.combine (List.map combineOn branches)
    When branches ->
      Result.map When <| List.combine (List.map combineWhen branches)
    Done -> Ok Done
