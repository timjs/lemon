module Lemon.Generate.Clean exposing (generate)

import Dict exposing (Dict)
import Helpers.Pretty exposing (..)
import Lemon.Name exposing (Fields, Name)
import Lemon.Syntax.Canonical exposing (Declaration(..), Expression(..), Module(..), Scope)
import Lemon.Syntax.Common.Atom as Atom exposing (Atom(..), Basic(..))
import Lemon.Syntax.Common.Pattern exposing (Alternative, Parameter, Pattern(..))
import Lemon.Syntax.Common.Statement as Statement exposing (Statement)
import Lemon.Syntax.Common.Type exposing (BasicType(..), Type(..))
import Pretty exposing (..)


todo = Debug.todo "todo"



-- Generate --------------------------------------------------------------------


generate : Module -> String
generate = pretty 100 << module_



-- Modules and declarations --


module_ : Module -> Doc
module_ (Module inner) =
  lines
    [ string preamble
    , empty
    , empty
    , scope inner
    ]


scope : Scope -> Doc
scope = Dict.foldl declaration empty


declaration : Name -> Declaration -> Doc -> Doc
declaration name (Value typ expr) doc =
  lines
    [ words [ string name, string "::", type_ typ ]
    , words [ string name, string "=" ]
    , indent 2 (expression expr)
    , empty
    ]



-- Expressions --


expression : Expression -> Doc
expression expr =
  todo



-- Atoms --


atom : Atom Expression -> Doc
atom a =
  case a of
    ABasic b ->
      basic b
    AVariable name ->
      string name
    AConstructor name exprs ->
      words <| string name :: List.map (parens << expression) exprs
    ARecord fields ->
      tuple <| List.map (expression << Tuple.second) fields


basic : Basic -> Doc
basic b =
  case b of
    Bool True ->
      string "True"
    Bool False ->
      string "False"
    Int i ->
      string <| String.fromInt i
    Float f ->
      string <| String.fromFloat f
    String s ->
      surround (char '"') (char '"') <| string s



-- Types --


type_ : Type -> Doc
type_ typ =
  case typ of
    TBasic b ->
      basicType b
    TVariable name ->
      string name
    TMaybe inner ->
      words [ string "Maybe", type_ inner ]
    TList inner ->
      words [ string "List", type_ inner ]
    TRecord fields ->
      tuple <| List.map (type_ << Tuple.second) fields
    TTask inner ->
      words [ string "Task", type_ inner ]
    TArrow left right ->
      words [ char '(', type_ left, char ')', string "->", type_ right ]


basicType : BasicType -> Doc
basicType b =
  case b of
    TBool -> string "Bool"
    TInt -> string "Int"
    TFloat -> string "Real"
    TString -> string "String"



-- Helpers ---------------------------------------------------------------------


tuple : List Doc -> Doc
tuple = parens << join (char ',')


list : List Doc -> Doc
list = brackets << join (char ',')



-- Preamble --------------------------------------------------------------------


preamble : String
preamble = """
module Main

import iTasks
import Data.List
import Data.Func


// Basic combinators ///////////////////////////////////////////////////////////

:: List a :== [a]
Cons x xs :== [x : xs]
End       :== []
null :== isEmpty

(|||) infixr 2 //:: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) l r x :== l x || r x

(&&&) infixr 3 //:: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) l r x :== l x && r x


// Editors //

view  label value :== viewInformation label [] value
edit  label value :== updateInformation label [] value
enter label       :== enterInformation label []

watch  label         store :== viewSharedInformation label [] store
change label         store :== updateSharedInformation label [] store
select label default store :== updateMultipleChoiceWithShared label [] store default


// Shares //

share  label value     :== sharedStore label value

($=) infixr 2 //:: ReadWriteShared r w -> (r -> w) -> Task w
($=) share fun :== upd fun share


// Steps //

ok :== const True

// Internal step
(>>>) infixl 1 //:: Task a -> List ( a -> Bool, a -> Task b ) -> Task b
(>>>) task options :== task >>* map trans options
where
  trans ( p, t ) = OnValue (ifValue p t)

// External step
(>?>) infixl 1 //:: Task a -> List ( String, a -> Bool, a -> Task b ) -> Task b
(>?>) task options :== task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)


// Parallels and Choices //

// Internal choice
(<|>) infixr 3 //:: Task a -> Task a -> Task a
(<|>) :== (-||-)

// External choice
(<?>) infixr 3 //:: Task a -> Task a -> Task a
(<?>) fst snd :== return () >?> [ ( "First" , ok, fst ), (  "Second", ok, snd ) ]

// Parallel
(<&>) infixr 4 //:: Task a -> Task b -> Task ( a, b )
(<&>) :== (-&&-)

// Parallel preference
(<&) infixl 4 //:: Task a -> Task b -> Task a
(<&) :== (-||)
(&>) infixr 4 //:: Task a -> Task b -> Task b
(&>) :== (||-)

// Fail
fail :: Task a
fail = transform (\\_ -> NoValue) (return ())
"""
