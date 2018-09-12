module Lemon.Parser exposing (parse)

import Lemon.Name exposing (Fields, Name)
import Lemon.Syntax.Abstract as Syntax exposing (..)
import Lemon.Syntax.Common.Atom as Syntax exposing (Atom(..), Basic(..))
import Lemon.Syntax.Common.Pattern as Syntax exposing (Alternative, Parameter, Pattern(..))
import Lemon.Syntax.Common.Statement as Syntax exposing (Statement(..))
import Lemon.Syntax.Common.Type as Syntax exposing (BasicType(..), Type(..))
import Parser exposing (..)
import Parser.Extras exposing (parens)
import Set exposing (Set)


parse : String -> Result (List DeadEnd) Module
parse = run module_



-- Modules ---------------------------------------------------------------------


module_ : Parser Module
module_ =
  succeed Module
    |. spaces
    |= scope
    |. spaces
    |. end


scope : Parser Scope
scope = by semicolon declaration


declaration : Parser Declaration
declaration =
  succeed Value
    |= lower
    |. colon
    |= type_
    |. semicolon
    |= lower
    |. spaces
    |= by spaces pattern
    |. spaces
    |. equals
    |= expression



-- Expressions -----------------------------------------------------------------


expression : Parser Expression
expression =
  let
    maybeCall expr =
      oneOf
        [ succeed (Call expr)
            |. backtrackable spaces
            |= by spaces expression
        , succeed expr
        ]
  in
  oneOf
    [ succeed Atom |= atom
    , succeed Lambda
        |. backslash
        |= by spaces parameter
        |. arrow
        |= lazy (\_ -> expression)
    , succeed Let
        |. keyword "let"
        |. spaces
        |= lazy (\_ -> scope)
        |. spaces
        |. keyword "in"
        |. spaces
        |= lazy (\_ -> expression)
    , succeed Case
        |. keyword "case"
        |. spaces
        |= lazy (\_ -> expression)
        |. spaces
        |. keyword "of"
        |. spaces
        |= by semicolon alternative
    , succeed If
        |. keyword "if"
        |. spaces
        |= lazy (\_ -> expression)
        |. spaces
        |. keyword "then"
        |. spaces
        |= lazy (\_ -> expression)
        |. spaces
        |. keyword "else"
        |. spaces
        |= lazy (\_ -> expression)
    , succeed Sequence
        |. keyword "do"
        |. spaces
        |= by semicolon statement
    , succeed identity
        |= parens (lazy (\_ -> expression))
    ]
    |> andThen maybeCall


parameter : Parser Parameter
parameter =
  succeed Tuple.pair
    |= pattern
    |. colon
    |= type_


alternative : Parser (Alternative Expression)
alternative =
  succeed Tuple.pair
    |= pattern
    |. arrow
    |= lazy (\_ -> expression)


statement : Parser (Statement Expression)
statement =
  let
    triple x y z =
      ( x, y, z )
  in
  oneOf
    [ succeed Set
        |. keyword "let"
        |. spaces
        |= pattern
        |. equals
        |= lazy (\_ -> expression)
    , succeed (\x xs -> Par (x :: xs))
        |. keyword "do"
        |. spaces
        |= by semicolon (lazy (\_ -> statement))
        |= by spaces
            (succeed identity
              |. keyword "also"
              |. spaces
              |= by semicolon (lazy (\_ -> statement))
            )
    , map On <|
        by spaces <|
          succeed triple
            |. keyword "on"
            |. spaces
            |= string
            |. spaces
            |. keyword "when"
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. keyword "do"
            |. spaces
            |= by semicolon (lazy (\_ -> statement))
    , map When <|
        by spaces <|
          succeed Tuple.pair
            |. keyword "when"
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. keyword "do"
            |. spaces
            |= by semicolon (lazy (\_ -> statement))
    , succeed Syntax.Done |. keyword "done"
    , succeed Bind
        |= pattern
        |. arrow
        |= lazy (\_ -> expression)
    , succeed Do
        |= lazy (\_ -> expression)
    ]



-- Atoms -----------------------------------------------------------------------


atom : Parser (Atom Expression)
atom =
  let
    --FIXME: Is this correct?
    desugar = List.foldr (\e es -> Syntax.cons e (Atom es)) Syntax.end
    args =
      succeed identity
        |. spaces
        |= by spaces (lazy (\_ -> expression))
  in
  oneOf
    [ succeed ABasic |= basic
    , succeed AVariable |= lower
    , succeed AConstructor |= upper |= optional [] args
    , succeed desugar |= list (lazy (\_ -> expression))
    , succeed ARecord |= record colon (lazy (\_ -> expression))
    ]


basic : Parser Basic
basic =
  oneOf
    [ succeed (Bool True) |. keyword "True"
    , succeed (Bool False) |. keyword "False"
    , succeed Int |= backtrackable int --NOTE: ints are like floats, we need to backtrack here

    -- , succeed Float |= float
    , succeed String |= string
    ]


int : Parser Int
int =
  oneOf
    [ succeed negate
        |. symbol "-"
        |= Parser.int
    , Parser.int
    ]


float : Parser Float
float =
  let
    inner =
      oneOf
        [ symbol "."
            |> andThen (always <| problem "floating point numbers must start with a digit, like 0.25")
        , Parser.float
        ]
  in
  oneOf
    [ succeed negate
        |. symbol "-"
        |= inner
    , inner
    ]


string : Parser String
string =
  succeed identity
    |. token "\""
    |= (chompWhile ((/=) '"') |> getChompedString)
    |. token "\""


list : Parser a -> Parser (List a)
list item =
  sequence
    { start = "["
    , separator = ","
    , end = "]"
    , spaces = spaces
    , item = item
    , trailing = Forbidden
    }


record : Parser () -> Parser a -> Parser (Fields a)
record sep item =
  sequence
    { start = "{"
    , separator = ","
    , end = "}"
    , spaces = spaces
    , item =
        succeed Tuple.pair
          |= lower
          |. sep
          |= item
    , trailing = Forbidden
    }



-- Patterns --------------------------------------------------------------------


pattern : Parser Pattern
pattern =
  oneOf
    [ succeed PBasic |= basic
    , succeed PVariable |= lower
    , succeed PConstructor |= upper |. spaces |= by spaces (lazy (\_ -> pattern))
    , succeed PRecord |= record equals (lazy (\_ -> pattern))
    , succeed PIgnore |. underscore
    , succeed identity |= parens (lazy (\_ -> pattern))
    ]



-- Types -----------------------------------------------------------------------


type_ : Parser Type
type_ =
  let
    maybeArrow left =
      oneOf
        [ succeed (TArrow left)
            |. backtrackable arrow
            |= type_
        , succeed left
        ]
  in
  oneOf
    [ succeed TBasic |= basicType
    , succeed TVariable |= lower
    , succeed TMaybe |. keyword "Maybe" |. spaces |= lazy (\_ -> type_)
    , succeed TList |. keyword "List" |. spaces |= lazy (\_ -> type_)
    , succeed TRecord |= record colon (lazy (\_ -> type_))
    , succeed TTask |. keyword "Task" |. spaces |= lazy (\_ -> type_)
    , succeed identity |= parens (lazy (\_ -> type_))
    ]
    |> andThen maybeArrow


basicType : Parser BasicType
basicType =
  oneOf
    [ succeed TBool |. keyword "Bool"
    , succeed TInt |. keyword "Int"
    , succeed TFloat |. keyword "Float"
    , succeed TString |. keyword "String"
    ]



-- Helpers ---------------------------------------------------------------------


some : Parser a -> Parser (List a)
some item =
  succeed (::)
    |= item
    |= many item


many : Parser a -> Parser (List a)
many item =
  let
    helper vs =
      oneOf
        [ succeed (\v -> Parser.Loop (v :: vs))
            |= item
        , succeed <| Parser.Done (List.reverse vs)
        ]
  in
  loop [] helper


optional : a -> Parser a -> Parser a
optional default item =
  oneOf
    [ item
    , succeed default
    ]


by : Parser () -> Parser a -> Parser (List a)
by sep item =
  let
    more =
      succeed identity
        |. backtrackable sep
        |= item
  in
  succeed (::)
    |= item
    |= many more



-- Symbols --


spacy : Parser () -> Parser ()
spacy item =
  succeed identity
    |. spaces
    |= item
    |. spaces


comma : Parser ()
comma = spacy <| symbol ","


colon : Parser ()
colon = spacy <| symbol ":"


semicolon : Parser ()
semicolon = spacy <| symbol ";"


equals : Parser ()
equals = spacy <| symbol "="


backslash : Parser ()
backslash = spacy <| symbol "\\"


arrow : Parser ()
arrow = spacy <| symbol "->"


underscore : Parser ()
underscore = spacy <| symbol "_"



-- Names --


keywords : Set String
keywords =
  Set.fromList
    [ "type"
    , "alias"
    , "let"
    , "in"
    , "case"
    , "of"
    , "if"
    , "then"
    , "else"
    , "do"
    , "also"
    , "on"
    , "when"
    , "done"
    ]


name : (Char -> Bool) -> Parser Name
name pred =
  variable
    { start = pred
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = keywords
    }


lower : Parser Name
lower = name Char.isLower


upper : Parser Name
upper = name Char.isUpper
