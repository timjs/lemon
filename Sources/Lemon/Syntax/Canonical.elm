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


hole = Debug.todo ""



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
      hole doExpression atom
    Abstract.Lambda locals body ->
      hole
    Abstract.Call func args ->
      hole
    Abstract.Let scope body ->
      hole
    Abstract.Case test alts ->
      hole
    Abstract.If test true false ->
      hole
    Abstract.Sequence stmts ->
      hole
