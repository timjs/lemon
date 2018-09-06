module Lemon.Parser exposing (parse)

import Flip exposing (..)
import Lemon.Name exposing (Name)
import Lemon.Syntax as Syntax exposing (..)
import Parser exposing (..)
import Parser.Expression
import Parser.Extras exposing (parens)
import Set exposing (Set)


parse : String -> Result (List DeadEnd) Module
parse = run module_



-- Modules ---------------------------------------------------------------------


module_ : Parser Module
module_ =
  succeed Module
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


alternative : Parser Alternative
alternative =
  succeed Tuple.pair
    |= pattern
    |. arrow
    |= lazy (\_ -> expression)


statement : Parser Statement
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


atom : Parser Atom
atom =
  oneOf
    [ succeed Basic |= basic
    , succeed Variable |= lower
    , succeed Some |. keyword "Some" |. spaces |= lazy (\_ -> expression)
    , succeed None |. keyword "None"
    , succeed List |= list (lazy (\_ -> expression))
    , succeed Record |= record colon (lazy (\_ -> expression))
    ]


basic : Parser Basic
basic =
  oneOf
    [ succeed (Bool True) |. keyword "True"
    , succeed (Bool False) |. keyword "False"
    , succeed Int |= backtrackable int --NOTE: ints are like floats, we need to backtrack here
    , succeed Float |= float
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
  let
    maybeCons left =
      oneOf
        [ succeed (PCons left)
            |. backtrackable doublecolon
            |= pattern
        , succeed left
        ]
  in
  oneOf
    [ succeed PBasic |= basic
    , succeed PVariable |= lower
    , succeed PSome |. keyword "Some" |. spaces |= lazy (\_ -> pattern)
    , succeed PNone |. keyword "None"
    , succeed PNil |. doublebracket
    , succeed PRecord |= record equals (lazy (\_ -> pattern))
    , succeed PIgnore |. underscore
    , succeed identity |= parens (lazy (\_ -> pattern))
    ]
    |> andThen maybeCons



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
    , succeed TVariable |= universal
    , succeed TOption |. keyword "option" |. spaces |= lazy (\_ -> type_)
    , succeed TList |. keyword "list" |. spaces |= lazy (\_ -> type_)
    , succeed TRecord |= record colon (lazy (\_ -> type_))
    , succeed TTask |. keyword "task" |. spaces |= lazy (\_ -> type_)
    , succeed identity |= parens (lazy (\_ -> type_))
    ]
    |> andThen maybeArrow


basicType : Parser BasicType
basicType =
  oneOf
    [ succeed TBool |. keyword "bool"
    , succeed TInt |. keyword "int"
    , succeed TFloat |. keyword "float"
    , succeed TString |. keyword "string"
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


doublecolon : Parser ()
doublecolon = spacy <| symbol "::"


doublebracket : Parser ()
doublebracket = spacy <| symbol "[]"



-- Names --


keywords : Set String
keywords =
  Set.fromList
    [ "type"
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


universal : Parser Name
universal =
  variable
    { start = (==) '\''
    , inner = Char.isLower
    , reserved = keywords
    }
