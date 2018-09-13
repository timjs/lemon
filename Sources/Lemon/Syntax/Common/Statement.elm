module Lemon.Syntax.Common.Statement exposing
  ( Statement(..)
  , combine
  , map
  , visible
  )

import Helpers.List as List
import Lemon.Names exposing (Name)
import Lemon.Syntax.Common.Pattern exposing (Pattern)


todo = Debug.todo "todo"


type Statement e
  = Set Pattern e
  | Bind Pattern e
  | Do e
  | Par (List (List (Statement e)))
  | When (List ( e, List (Statement e) ))
  | On (List ( Name, e, List (Statement e) ))
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
    When branches ->
      When <| List.map mapWhen branches
    On branches ->
      On <| List.map mapOn branches
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
    When branches ->
      Result.map When <| List.combine (List.map combineWhen branches)
    On branches ->
      Result.map On <| List.combine (List.map combineOn branches)
    Done -> Ok Done



--FIXME: Is this doable to do on the tree or should we do this during canonicalisation and save it?


visible : Statement e -> List Name
visible statement =
  todo
