module Lemon.Generate.Clean exposing (generate)

import Dict exposing (Dict)
import Helpers.Pretty exposing (..)
import Lemon.Name exposing (Name)
import Lemon.Syntax.Canonical exposing (Declaration(..), Expression(..), Module(..), Scope)
import Lemon.Syntax.Common.Atom as Atom exposing (Atom(..), Basic(..), Fields)
import Lemon.Syntax.Common.Pattern exposing (Alternative, Parameter, Pattern(..))
import Lemon.Syntax.Common.Statement as Statement exposing (Statement(..))
import Lemon.Syntax.Common.Type exposing (BasicType(..), Type(..))
import Pretty exposing (..)



-- Generate --------------------------------------------------------------------


generate : Module -> String
generate = pretty 100 << module_



-- Modules and declarations --


module_ : Module -> Doc
module_ (Module inner) =
  lines
    [ string preamble
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
    , indent <| expression expr
    , empty
    ]



-- Expressions --


expression : Expression -> Doc
expression expr =
  case expr of
    Atom a ->
      atom a
    Lambda ( pat, _ ) inner ->
      parens <| words [ char '\\', pattern pat, string "->", expression inner ]
    Call func arg ->
      words [ parens <| expression func, parens <| expression arg ]
    Let decls inner ->
      lines
        [ string "let"
        , indent <| scope decls
        , string "in"
        , expression inner
        ]
    Case match alts ->
      lines
        [ words [ string "case", expression match, string "of" ]
        , indent <| List.foldl alternative empty alts
        ]
    Sequence stmts ->
      parens <| lines <| List.map statement stmts



-- Statements --


statement : Statement Expression -> Doc
statement stmt =
  let
    bind pat expr =
      words [ expression expr, string ">>=", char '\\', pattern pat, string "->" ]
    pair2 ( pred, stmts ) =
      tuple [ expression pred, statement stmt ]
    pair3 ( name, pred, stmts ) =
      tuple [ string name, expression pred, statement stmt ]
    par stmts =
      lines
        [ string "also"
        , indent <| lines <| List.map statement stmts
        ]
    some_tuple_with_visible_names = Debug.todo "some tuple with visible names"
  in
  case stmt of
    Set pat expr ->
      words [ string "let", pattern pat, char '=', expression expr, string "in" ]
    Bind pat expr ->
      bind pat expr
    Do expr ->
      bind PIgnore expr
    Par brns ->
      lines <| List.map par brns
    When brns ->
      lines
        [ string ">>>"
        , indent <| list <| List.map pair2 brns
        ]
    On brns ->
      lines
        [ string ">?>"
        , indent <| list <| List.map pair3 brns
        ]
    Done ->
      --FIXME: How to handle this?
      words [ string "return", tuple <| List.map string <| some_tuple_with_visible_names ]



-- Atoms --


atom : Atom Expression -> Doc
atom atm =
  case atm of
    ABasic bas ->
      basic bas
    AVariable name ->
      string name
    AJust inner ->
      just expression inner
    ANothing -> nothing
    ACons head tail ->
      cons expression head tail
    ANil -> nil
    ARecord fields ->
      tuple <| List.map (expression << Tuple.second) fields


basic : Basic -> Doc
basic bas =
  case bas of
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


cons conv head tail =
  brackets <| words [ conv head, char ':', conv tail ]


nil = string "[]"


just conv inner =
  words [ string "Just", parens <| conv inner ]


nothing = string "Nothing"



-- Patterns --


pattern : Pattern -> Doc
pattern pat =
  case pat of
    PBasic bas ->
      basic bas
    PVariable name ->
      string name
    PJust inner ->
      just pattern inner
    PNothing -> nothing
    PCons head tail ->
      cons pattern head tail
    PNil -> nil
    PRecord fields ->
      tuple <| List.map (string << Tuple.first) fields
    PIgnore -> char '_'


alternative : Alternative Expression -> Doc -> Doc
alternative ( pat, expr ) doc =
  lines
    [ words [ pattern pat, string "->" ]
    , indent <| expression expr
    ]



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
      words [ parens <| type_ left, string "->", type_ right ]


basicType : BasicType -> Doc
basicType b =
  case b of
    TBool -> string "Bool"
    TInt -> string "Int"
    TFloat -> string "Real"
    TString -> string "String"



-- Helpers ---------------------------------------------------------------------


indent : Doc -> Doc
indent = Pretty.indent 2


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


// Preamble ////////////////////////////////////////////////////////////////////

:: List a :== [a]
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


// Code ////////////////////////////////////////////////////////////////////////

"""
