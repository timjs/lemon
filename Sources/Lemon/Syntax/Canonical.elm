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
import Lemon.Names exposing (Name)
import Lemon.Syntax.Common.Atom as Atom exposing (Atom(..), Basic(..))
import Lemon.Syntax.Common.Pattern exposing (Alternative, Parameter, Pattern(..))
import Lemon.Syntax.Common.Statement as Statement exposing (Statement)
import Lemon.Syntax.Fundamental as Fundamental
import Lemon.Types exposing (Type(..))
import Result.Extra as Result



-- Modules and declarations ----------------------------------------------------


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


canonicalise : Fundamental.Module -> Result Error Module
canonicalise (Fundamental.Module scope) =
  Result.map Module <| doScope scope


doScope : Fundamental.Scope -> Result Error Scope
doScope = List.map doDeclaration >> List.combine >> Result.map Dict.fromList


doDeclaration : Fundamental.Declaration -> Result Error ( Name, Declaration )
doDeclaration (Fundamental.Value name1 typ name2 params body) =
  if name1 == name2 then
    doBody typ params body
      |> Result.map (\res -> ( name1, Value typ res ))
  else
    Err <| BadNaming name1 name2


doBody : Type -> List Pattern -> Fundamental.Expression -> Result Error Expression
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


doExpression : Fundamental.Expression -> Result Error Expression
doExpression expr =
  case expr of
    Fundamental.Atom atom ->
      Result.map Atom
        (Atom.combine <| Atom.map doExpression atom)
    Fundamental.Lambda params body ->
      Result.map2 (List.foldr Lambda)
        (doExpression body)
        (Ok params)
    Fundamental.Call func args ->
      Result.map2 (List.foldl Call)
        (doExpression func)
        (List.combine <| List.map doExpression args)
    Fundamental.Let scope body ->
      Result.map2 Let
        (doScope scope)
        (doExpression body)
    Fundamental.Case test alts ->
      Result.map2 Case
        (doExpression test)
        (List.combine <| List.map (Tuple.combineSecond << Tuple.mapSecond doExpression) alts)
    Fundamental.If test true false ->
      Result.map3 mkIf
        (doExpression test)
        (doExpression true)
        (doExpression false)
    Fundamental.Sequence stmts ->
      Result.map Sequence
        (List.combine <| List.map (Statement.combine << Statement.map doExpression) stmts)
