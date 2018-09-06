module Lemon.Parser exposing (parse)

import Dict exposing (Dict)
import Lemon.Name exposing (Name)
import Lemon.Syntax as Syntax exposing (..)
import Parser exposing (..)
import Parser.Expression
import Set exposing (Set)


parse : String -> Result (List DeadEnd) Module
parse = run module_



-- Modules ---------------------------------------------------------------------


module_ : Parser Module
module_ = map Module scope


scope : Parser Scope
scope = map Dict.fromList <| chain ";" declaration


declaration : Parser ( Name, Declaration )
declaration =
  let
    quadruple a b c d =
      ( ( a, b ), ( c, d ) )
    check ( ( name1, annot ), ( name2, body ) ) =
      if name1 == name2 then
        succeed ( name1, Value annot body )
      else
        problem <| "value names do not match: `" ++ name2 ++ "` should be `" ++ name1 ++ "`"
    run =
      succeed quadruple
        |= lower
        |. colon
        |= type_
        |. semicolon
        |= lower
        |. equals
        |= expression
  in
  run
    |> andThen check



-- Expressions -----------------------------------------------------------------


expression : Parser Expression
expression =
  oneOf
    [ succeed Atom |= atom
    , succeed Lambda
        |. backslash
        |= pattern
        |. colon
        |= type_
        |. arrow
        |= lazy (\_ -> expression)

    --FIXME
    -- , succeed Call
    --     |= lazy (\_ -> expression)
    --     |= lazy (\_ -> expression)
    , succeed Let
        |. keyword "let"
        |= lazy (\_ -> scope)
        |. keyword "in"
        |= lazy (\_ -> expression)
    , succeed Case
        |. keyword "case"
        |= lazy (\_ -> expression)
        |. keyword "of"
        |= chain ";" alternative
    , succeed If
        |. keyword "if"
        |= lazy (\_ -> expression)
        |. keyword "then"
        |= lazy (\_ -> expression)
        |. keyword "else"
        |= lazy (\_ -> expression)
    , succeed Sequence
        |. keyword "do"
        |= chain ";" statement
    ]


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
        |= pattern
        |. equals
        |= lazy (\_ -> expression)
    , succeed (\x xs -> Par (x :: xs))
        |. keyword "do"
        |= chain ";" (lazy (\_ -> statement))
        |= chain " "
            (succeed identity
              |. keyword "also"
              |= chain ";" (lazy (\_ -> statement))
            )
    , map On <|
        chain " " <|
          succeed triple
            |. keyword "on"
            |= string
            |. keyword "when"
            |= lazy (\_ -> expression)
            |. keyword "do"
            |= chain ";" (lazy (\_ -> statement))
    , map When <|
        chain " " <|
          succeed Tuple.pair
            |. keyword "when"
            |= lazy (\_ -> expression)
            |. keyword "do"
            |= chain ";" (lazy (\_ -> statement))
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
    , succeed Record |= dict colon (lazy (\_ -> expression))
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



-- Patterns --------------------------------------------------------------------


pattern : Parser Pattern
pattern =
  oneOf
    [ succeed PBasic |= basic
    , succeed PVariable |= lower
    , succeed PSome |. keyword "Some" |= lazy (\_ -> pattern)
    , succeed PNone |. keyword "None"
    , succeed PCons |= lazy (\_ -> pattern) |. doublecolon |= lazy (\_ -> pattern)
    , succeed PNil |. doublebracket
    , succeed PRecord |= dict equals (lazy (\_ -> pattern))
    , succeed PIgnore |. underscore
    ]



-- Types -----------------------------------------------------------------------


type_ : Parser Type
type_ =
  oneOf
    [ succeed TBasic |= basicType
    , succeed TVariable |= universal
    , succeed TOption |. keyword "option" |= lazy (\_ -> type_)
    , succeed TList |. keyword "list" |= lazy (\_ -> type_)
    , succeed TRecord |= dict colon (lazy (\_ -> type_))
    , succeed TTask |. keyword "task" |= lazy (\_ -> type_)

    --FIXME
    --, succeed TArrow |= lazy (\_ -> type_) |. arrow |= lazy (\_ -> type_)
    ]


basicType : Parser BasicType
basicType =
  oneOf
    [ succeed TBool |. keyword "bool"
    , succeed TInt |. keyword "int"
    , succeed TFloat |. keyword "float"
    , succeed TString |. keyword "string"
    ]



-- Helpers ---------------------------------------------------------------------
-- Sequences --


chain : String -> Parser a -> Parser (List a)
chain sep item =
  sequence
    { start = ""
    , separator = sep
    , end = ""
    , spaces = spaces
    , item = item
    , trailing = Forbidden
    }


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


dict : Parser () -> Parser a -> Parser (Dict Name a)
dict sep item =
  map Dict.fromList <|
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



-- Symbols --


spacy : Parser () -> Parser ()
spacy item =
  succeed identity
    |. spaces
    |= item
    |. spaces


colon : Parser ()
colon = spacy <| symbol ":"


semicolon : Parser ()
semicolon = spacy <| symbol ";"


equals : Parser ()
equals = spacy <| symbol "="


underscore : Parser ()
underscore = spacy <| symbol "_"


backslash : Parser ()
backslash = spacy <| symbol "\\"


arrow : Parser ()
arrow = spacy <| symbol "->"


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
