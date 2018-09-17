module Lemon.Canonicalise exposing (canonicalise)

import Dict exposing (Dict)
import Helpers.List as List
import Helpers.Tuple as Tuple
import Lemon.Names exposing (..)
import Lemon.Syntax.Canonical exposing (..)
import Lemon.Syntax.Textual as Textual
import Lemon.Types exposing (..)
import Result.Extra as Result


hole = Debug.todo "todo"



-- Errors ----------------------------------------------------------------------


type Error
  = BadParameters Type (List Pattern)
  | BadNaming Name Name



-- Canonicalise ----------------------------------------------------------------


canonicalise : Textual.Module -> Result Error Module
canonicalise (Textual.Module inner) =
  Result.map Module <| scope inner


scope : Textual.Scope -> Result Error Scope
scope = List.map declaration >> List.combine >> Result.map Dict.fromList


declaration : Textual.Declaration -> Result Error ( Name, Declaration )
declaration (Textual.Value name1 typ name2 params inner) =
  if name1 == name2 then
    body typ params inner
      |> Result.map (\res -> ( name1, Value typ res ))
  else
    Err <| BadNaming name1 name2


body : Type -> List Pattern -> Textual.Expression -> Result Error Expression
body typ params inner =
  let
    go arrow patterns expr =
      case ( typ, patterns ) of
        ( TArrow this rest, pat :: pats ) ->
          go rest pats <| Lambda ( pat, this ) expr
        ( _, pat :: pats ) ->
          Err <| BadParameters typ params
        ( TArrow this rest, [] ) ->
          Err <| BadParameters typ params
        ( _, [] ) ->
          Ok expr
  in
  expression inner |> Result.andThen (go typ params)


expression : Textual.Expression -> Result Error Expression
expression expr =
  hole



{-
   case expr of
     Textual.Atom atom ->
       Result.map Atom
         (Atom.combine <| Atom.map expression atom)
     Textual.Lambda params body ->
       Result.map2 (List.foldr Lambda)
         (expression body)
         (Ok params)
     Textual.Call func args ->
       Result.map2 (List.foldl Call)
         (expression func)
         (List.combine <| List.map expression args)
     Textual.Let scope body ->
       Result.map2 Let
         (scope scope)
         (expression body)
     Textual.Case test alts ->
       Result.map2 Case
         (expression test)
         (List.combine <| List.map (Tuple.combineSecond << Tuple.mapSecond expression) alts)
     Textual.If test true false ->
       Result.map3 mkIf
         (expression test)
         (expression true)
         (expression false)
     Textual.Sequence stmts ->
       Result.map Sequence
         (List.combine <| List.map (Statement.combine << Statement.map expression) stmts)
-}
