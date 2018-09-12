module Lemon.Syntax.Canonical exposing
  ( Declaration(..)
  , Error(..)
  , Expression(..)
  , Module(..)
  , Scope
  , canonicalise
  , empty
  )

import Dict exposing (Dict)
import Helpers.List as List
import Helpers.Tuple as Tuple
import Lemon.Name exposing (Name)
import Lemon.Syntax.Abstract as Abstract
import Lemon.Syntax.Common exposing (..)
import Lemon.Syntax.Common.Atom as Atom exposing (..)
import Result.Extra as Result


type Hole
  = Hole



-- Modules and Definitions -----------------------------------------------------


type Module
  = Module Scope


type alias Scope =
  Dict Name Declaration


type Declaration
  = Value Type Expression



-- Expressions -----------------------------------------------------------------


type Expression
  = Atom (Atom Expression)
  | Lambda Parameter Expression
  | Call Expression Expression
  | Let Scope Expression
  | Case Expression (List (Alternative Expression))
  | Sequence (List (Statement Expression))



-- Init ------------------------------------------------------------------------


empty : Scope
empty = Dict.empty


mkIf : Expression -> Expression -> Expression -> Expression
mkIf test true false =
  Case test <|
    [ ( PBasic (Bool True), true )
    , ( PBasic (Bool False), false )
    ]



-- Canonicalise ----------------------------------------------------------------


type Error
  = BadParameters Type (List Pattern)
  | BadNaming Name Name


canonicalise : Abstract.Module -> Result Error Module
canonicalise (Abstract.Module scope) =
  Result.map Module <| doScope scope


doScope : Abstract.Scope -> Result Error Scope
doScope = List.map doDeclaration >> List.combine >> Result.map Dict.fromList


doDeclaration : Abstract.Declaration -> Result Error ( Name, Declaration )
doDeclaration (Abstract.Value name1 typ name2 params body) =
  if name1 == name2 then
    doBody typ params body
      |> Result.map (\res -> ( name1, Value typ res ))
  else
    Err <| BadNaming name1 name2


doBody : Type -> List Pattern -> Abstract.Expression -> Result Error Expression
doBody typ params body =
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
  doExpression body |> Result.andThen (go typ params)


doExpression : Abstract.Expression -> Result Error Expression
doExpression expr =
  case expr of
    Abstract.Atom atom ->
      Result.map Atom
        (Atom.combine <| Atom.map doExpression atom)
    Abstract.Lambda params body ->
      Result.map2 (List.foldr Lambda)
        (doExpression body)
        (Ok params)
    Abstract.Call func args ->
      Result.map2 (List.foldl Call)
        (doExpression func)
        (List.combine <| List.map doExpression args)
    Abstract.Let scope body ->
      Result.map2 Let
        (doScope scope)
        (doExpression body)
    Abstract.Case test alts ->
      Result.map2 Case
        (doExpression test)
        (List.combine <| List.map (Tuple.combineSecond << Tuple.mapSecond doExpression) alts)
    Abstract.If test true false ->
      Result.map3 mkIf
        (doExpression test)
        (doExpression true)
        (doExpression false)
    Abstract.Sequence stmts ->
      Result.map Sequence
        (List.combine <| List.map (statement_combine << statement_map doExpression) stmts)


statement_combine : Statement (Result x e) -> Result x (Statement e)
statement_combine stmt =
  Hole


statement_map : (a -> b) -> Statement a -> Statement b
statement_map function aStatement =
  Hole
