module Lemon.Parser exposing (parse)

import Dict exposing (Dict)
import Flip exposing (..)
import Lemon.Name exposing (Name)
import Lemon.Syntax as Syntax exposing (..)
import Parser exposing (..)
import Parser.Expression
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
scope = map Dict.fromList <| chain semicolon declaration


declaration : Parser ( Name, Declaration )
declaration =
  let
    group name1 annot name2 params body =
      { name1 = name1, annot = annot, name2 = name2, params = params, body = body }
    check { name1, annot, name2, params, body } =
      if name1 == name2 then
        succeed ( name1, Value annot params body )
      else
        problem <| "value names do not match: `" ++ name2 ++ "` should be `" ++ name1 ++ "`"
    run =
      succeed group
        |= lower
        |. colon
        |= type_
        |. semicolon
        |= lower
        |. spaces
        |= chain spaces pattern
        |. spaces
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
        |= chain spaces parameter
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
        |= chain semicolon alternative
    , succeed If
        |. keyword "if"
        |= lazy (\_ -> expression)
        |. keyword "then"
        |= lazy (\_ -> expression)
        |. keyword "else"
        |= lazy (\_ -> expression)
    , succeed Sequence
        |. keyword "do"
        |= chain semicolon statement
    ]


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
        |= pattern
        |. equals
        |= lazy (\_ -> expression)
    , succeed (\x xs -> Par (x :: xs))
        |. keyword "do"
        |= chain semicolon (lazy (\_ -> statement))
        |= chain spaces
            (succeed identity
              |. keyword "also"
              |= chain semicolon (lazy (\_ -> statement))
            )
    , map On <|
        chain spaces <|
          succeed triple
            |. keyword "on"
            |= string
            |. keyword "when"
            |= lazy (\_ -> expression)
            |. keyword "do"
            |= chain semicolon (lazy (\_ -> statement))
    , map When <|
        chain spaces <|
          succeed Tuple.pair
            |. keyword "when"
            |= lazy (\_ -> expression)
            |. keyword "do"
            |= chain semicolon (lazy (\_ -> statement))
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

    --FIXME
    -- , succeed PCons |= lazy (\_ -> pattern) |. spacy doublecolon |= lazy (\_ -> pattern)
    , succeed PNil |. doublebracket
    , succeed PRecord |= dict equals (lazy (\_ -> pattern))
    , succeed PIgnore |. underscore
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
    , succeed TVariable |= universal
    , succeed TOption |. keyword "option" |. spaces |= lazy (\_ -> type_)
    , succeed TList |. keyword "list" |. spaces |= lazy (\_ -> type_)
    , succeed TRecord |= dict colon (lazy (\_ -> type_))
    , succeed TTask |. keyword "task" |. spaces |= lazy (\_ -> type_)
    ]
    |> andThen maybeArrow



--FIXME
--, succeed TArrow |= lazy (\_ -> type_) |. arrow |= lazy (\_ -> type_)


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


chain : Parser () -> Parser a -> Parser (List a)
chain sep item =
  let
    more =
      succeed identity
        |. backtrackable sep
        |= item
  in
  succeed (::)
    |= item
    |= many more


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
