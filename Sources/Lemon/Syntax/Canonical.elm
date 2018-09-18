module Lemon.Syntax.Canonical exposing
  ( Alternative
  , Atom(..)
  , Branch
  , Declaration(..)
  , Expression(..)
  , Fields
  , Flow(..)
  , Module(..)
  , Parameter
  , Pattern(..)
  , Scope
  , Step(..)
  , Task(..)
  , empty
  , ifThenElse
  , ok
  )

import Dict exposing (Dict)
import Lemon.Names exposing (..)
import Lemon.Syntax.Common exposing (..)
import Lemon.Types exposing (..)



-- Modules and declarations ----------------------------------------------------


type Module
  = Module Scope


type alias Scope =
  Dict Name Declaration


type Declaration
  = Value Type Expression



-- Expressions -----------------------------------------------------------------


type Expression
  = Atom Atom
  | Lambda Parameter Expression
  | Call Expression Expression
  | Let Scope Expression
  | Case Expression (List Alternative)
  | Sequence Flow


type alias Alternative =
  ( Pattern, Expression )



-- Atoms -----------------------------------------------------------------------


type Atom
  = ABasic Basic
  | AVariable Name
  | AJust Expression
  | ANothing
  | ACons Expression Expression
  | ANil
  | ARecord (Fields Expression)


type alias Fields a =
  Dict Name a



-- Flows -----------------------------------------------------------------------


type Flow
  = Flow Task Step


type Task
  = Do Pattern Expression --TODO: add explicit arguments?
  | Par (List Flow)


type Step
  = When (List Branch)
  | On (List ( Name, Branch ))
  | Done


type alias Branch =
  ( Expression, Flow )



-- Patterns --------------------------------------------------------------------


type Pattern
  = PBasic Basic
  | PVariable Name
  | PJust Pattern
  | PNothing
  | PCons Pattern Pattern
  | PNil
  | PRecord (Fields Pattern)
  | PIgnore


type alias Parameter =
  ( Pattern, Type )



-- Init ------------------------------------------------------------------------


empty : Scope
empty = Dict.empty


ifThenElse : Expression -> Expression -> Expression -> Expression
ifThenElse test true false =
  Case test <|
    [ ( PBasic (Bool True), true )
    , ( PBasic (Bool False), false )
    ]


ok : Expression
ok = Atom (ABasic (Bool True))
