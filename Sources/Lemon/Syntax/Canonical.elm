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
import Helpers
import Lemon.Name exposing (Name)
import Lemon.Syntax.Abstract as Abstract
import Lemon.Syntax.Common exposing (..)
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



-- Canonicalise ----------------------------------------------------------------


type Error
  = Redefinition Name Name
  | Duplication Name
  | Disagreement Name Name


canonicalise : Abstract.Module -> Result Error Module
canonicalise (Abstract.Module scope) =
  Result.map Module <| doScope scope


doScope : Abstract.Scope -> Result Error Scope
doScope = List.map doDeclaration >> Helpers.combine >> Result.map Dict.fromList


doDeclaration : Abstract.Declaration -> Result Error ( Name, Declaration )
doDeclaration (Abstract.Value name1 annot name2 params body) =
  if name1 == name2 then
    doBody annot params body
      |> Result.map (\res -> ( name1, Value annot res ))
  else
    Err <| Disagreement name1 name2


doBody : Type -> List Pattern -> Abstract.Expression -> Result Error Expression
doBody annot params body =
  --FIXME: eta-reduct parameters
  doExpression body


doExpression : Abstract.Expression -> Result Error Expression
doExpression expr =
  case expr of
    Abstract.Atom atom ->
      Result.map Atom <| atom_combine <| atom_map doExpression atom
    Abstract.Lambda params body ->
      Result.map2 (List.foldr Lambda)
        (doExpression expr)
        (Ok params)
    Abstract.Call func args ->
      Result.map2 (List.foldl Call)
        (doExpression func)
        (Result.combine <| List.map doExpression args)
    Abstract.Let scope body ->
      Result.map2 Let
        (doScope scope)
        (doExpression body)
    Abstract.Case test alts ->
      Result.map2 Case
        (doExpression test)
        (Result.combine <| List.map (tuple_combine << Tuple.mapSecond doExpression) alts)
    Abstract.If test true false ->
      Hole
    Abstract.Sequence stmts ->
      Hole


tuple_combine : ( a, Result x b ) -> Result x ( a, b )
tuple_combine ( a, rb ) =
  case rb of
    Ok b ->
      Ok ( a, b )
    Err x ->
      Err x
