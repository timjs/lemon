module Lemon.Canonicalise exposing (canonicalise)

import Dict exposing (Dict)
import Helpers.Result exposing (..)
import Lemon.Names exposing (..)
import Lemon.Syntax.Canonical exposing (..)
import Lemon.Syntax.Textual as Textual
import Lemon.Types exposing (..)
import Result exposing (..)
import Result.Extra exposing (..)



-- Errors ----------------------------------------------------------------------


type Error
  = BadParameters Type (List Pattern)
  | BadNaming Name Name
  | NeedTask Textual.Statement
  | NeedExpression
  | BadBranching Textual.Statement



-- Canonicalise ----------------------------------------------------------------


canonicalise : Textual.Module -> Result Error Module
canonicalise = doModule



-- Modules and declarations --


doModule : Textual.Module -> Result Error Module
doModule (Textual.Module inner) =
  map Module
    (doScope inner)


doScope : Textual.Scope -> Result Error Scope
doScope = List.map doDeclaration >> combine >> map Dict.fromList


doDeclaration : Textual.Declaration -> Result Error ( Name, Declaration )
doDeclaration (Textual.Value name1 tipe name2 params body) =
  if name1 == name2 then
    map2 (\tipe_new body_new -> ( name1, Value tipe_new body_new ))
      (doType tipe)
      (doBody tipe params body)
  else
    Err <| BadNaming name1 name2


doBody : Textual.Type -> List Textual.Pattern -> Textual.Expression -> Result Error Expression
doBody tipe params inner =
  let
    go : Type -> List Pattern -> Expression -> Result Error Expression
    go arrow patterns expr =
      case ( arrow, patterns ) of
        ( TArrow this rest, pat :: pats ) -> go rest pats <| Lambda ( pat, this ) expr
        ( _, pat :: pats ) -> Err <| BadParameters arrow patterns
        ( TArrow this rest, [] ) -> Err <| BadParameters arrow patterns
        ( _, [] ) -> Ok expr
  in
  join <|
    map3 go
      (doType tipe)
      (combineMap doPattern params)
      (doExpression inner)



-- Expressions --


doExpression : Textual.Expression -> Result Error Expression
doExpression expr =
  case expr of
    Textual.Atom atom ->
      map Atom
        (doAtom atom)
    Textual.Lambda params body ->
      map2 (List.foldr Lambda)
        (doExpression body)
        (combineMap (combineMapBoth doPattern doType) params)
    Textual.Call func args ->
      map2 (List.foldl Call)
        (doExpression func)
        (combineMap doExpression args)
    Textual.Let scope body ->
      map2 Let
        (doScope scope)
        (doExpression body)
    Textual.Case test alts ->
      map2 Case
        (doExpression test)
        (combineMap (combineMapBoth doPattern doExpression) alts)
    Textual.If test true false ->
      map3 ifThenElse
        (doExpression test)
        (doExpression true)
        (doExpression false)
    Textual.Sequence stmts ->
      map Sequence
        (doStatements stmts)



-- Atoms --


doAtom : Textual.Atom -> Result Error Atom
doAtom atom =
  case atom of
    Textual.ABasic basic ->
      Ok <| ABasic basic
    Textual.AVariable name ->
      Ok <| AVariable name
    Textual.AJust expr ->
      map AJust
        (doExpression expr)
    Textual.ANothing ->
      Ok <| ANothing
    Textual.ACons head tail ->
      map2 ACons
        (doExpression head)
        (doExpression tail)
    Textual.ANil ->
      Ok <| ANil
    Textual.ARecord fields ->
      map (ARecord << Dict.fromList)
        (combineMap (combineMapSecond doExpression) fields)



-- Statements --


doStatements : List Textual.Statement -> Result Error Flow
doStatements = needTask


needTask : List Textual.Statement -> Result Error Flow
needTask stmts =
  case stmts of
    (Textual.SLet pat expr) :: rest ->
      Debug.todo "Encountered let during statement canonicalisation"
    -- Tasks
    (Textual.SBind pat expr) :: rest ->
      map3 (\p e s -> Flow (Do p e) s)
        (doPattern pat)
        (doExpression expr)
        (needStep True rest)
    (Textual.SIgnore expr) :: rest ->
      map2 (\e s -> Flow (Do PIgnore e) s)
        (doExpression expr)
        (needStep False rest)
    (Textual.SPar forks) :: rest ->
      map2 (\fs s -> Flow (Par fs) s)
        (combineMap doStatements forks)
        (needStep False rest)
    -- Steps
    ((Textual.SWhen _) as stmt) :: _ ->
      Err <| NeedTask stmt
    ((Textual.SOn _) as stmt) :: _ ->
      Err <| NeedTask stmt
    (Textual.SDone as stmt) :: _ ->
      Err <| NeedTask stmt
    [] ->
      Err <| NeedExpression


needStep : Bool -> List Textual.Statement -> Result Error Step
needStep desperately stmts =
  let
    simple =
      map (\t -> When [ ( ok, t ) ])
        (needTask stmts)
  in
  case stmts of
    (Textual.SLet pat expr) :: rest ->
      Debug.todo "Encountered let during statement canonicalisation"
    -- Tasks
    --FIXME: Add match on interactive tasks...
    (Textual.SBind pat expr) :: rest ->
      simple
    (Textual.SIgnore expr) :: rest ->
      simple
    (Textual.SPar branches) :: rest ->
      simple
    -- Steps
    (Textual.SWhen branches) :: [] ->
      map When
        (combineMap (combineMapBoth doExpression doStatements) branches)
    ((Textual.SWhen _) as stmt) :: _ ->
      Err <| BadBranching stmt
    (Textual.SOn actions) :: [] ->
      map On
        (combineMap (combineMapSecond (combineMapBoth doExpression doStatements)) actions)
    ((Textual.SOn _) as stmt) :: _ ->
      Err <| BadBranching stmt
    Textual.SDone :: rest ->
      Ok Done
    [] ->
      if desperately then
        Err NeedExpression
      else
        Ok Done



-- Patterns --


doPattern : Textual.Pattern -> Result Error Pattern
doPattern pat =
  case pat of
    Textual.PBasic basic ->
      Ok <| PBasic basic
    Textual.PVariable name ->
      Ok <| PVariable name
    Textual.PJust inner ->
      map PJust
        (doPattern inner)
    Textual.PNothing ->
      Ok <| PNothing
    Textual.PCons head tail ->
      map2 PCons
        (doPattern head)
        (doPattern tail)
    Textual.PNil ->
      Ok <| PNil
    Textual.PRecord fields ->
      map (PRecord << Dict.fromList)
        (combineMap (combineMapSecond doPattern) fields)
    Textual.PIgnore ->
      Ok <| PIgnore



-- Types --


doType : Textual.Type -> Result Error Type
doType tipe =
  case tipe of
    Textual.TBasic basic ->
      Ok <| TBasic basic
    Textual.TVariable name ->
      Ok <| TVariable name
    Textual.TMaybe inner ->
      map TMaybe
        (doType inner)
    Textual.TList inner ->
      map TList
        (doType inner)
    Textual.TRecord fields ->
      map (TRecord << Dict.fromList)
        (combineMap (combineMapSecond doType) fields)
    Textual.TTask inner ->
      map TTask
        (doType inner)
    Textual.TArrow left right ->
      map2 TArrow
        (doType left)
        (doType right)
