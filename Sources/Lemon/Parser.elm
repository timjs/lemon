module Flow.Parser exposing (parse)

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
      ( a, ( b, ( c, d ) ) )
    check ( name1, ( annot, ( name2, body ) ) ) =
      if name1 == name2 then
        succeed ( name1, Value annot body )
      else
        problem <| "names do not match:" ++ name1 ++ " and " ++ name2
    run =
      succeed quadruple
        |= lower
        |. colon
        |= type_ ()
        |. semicolon
        |= lower
        |. equals
        |= expression ()
  in
  run
    |> andThen check



-- Expressions -----------------------------------------------------------------


expression : () -> Parser Expression
expression () =
  oneOf
    [ succeed Atom |= atom
    , succeed Lambda
        |. backslash
        |= pattern ()
        |. colon
        |= type_ ()
        |. arrow
        |= lazy expression
    , succeed Call
        |= lazy expression
        |= lazy expression
    , succeed Let
        |= scope
        |= lazy expression
    , succeed Case
        |. keyword "case"
        |= lazy expression
        |. keyword "of"
        |= chain ";" (alternative ())
    , succeed If
        |. keyword "if"
        |= lazy expression
        |. keyword "then"
        |= lazy expression
        |. keyword "else"
        |= lazy expression
    , succeed Sequence
        |. keyword "do"
        |= chain ";" (statement ())
    ]


alternative : () -> Parser Alternative
alternative () =
  succeed Tuple.pair
    |= pattern ()
    |. arrow
    |= lazy expression


statement : () -> Parser Statement
statement () =
  let
    triple x y z =
      ( x, y, z )
  in
  oneOf
    [ succeed Set
        |. keyword "let"
        |= pattern ()
        |. equals
        |= lazy expression
    , succeed (\x xs -> Par (x :: xs))
        |. keyword "do"
        |= chain ";" (lazy statement)
        |= chain " "
            (succeed identity
              |. keyword "also"
              |= chain ";" (lazy statement)
            )
    , map On <|
        chain " " <|
          succeed triple
            |. keyword "on"
            |= string
            |. keyword "when"
            |= lazy expression
            |. keyword "do"
            |= chain ";" (lazy statement)
    , map When <|
        chain " " <|
          succeed Tuple.pair
            |. keyword "when"
            |= lazy expression
            |. keyword "do"
            |= chain ";" (lazy statement)
    , succeed Syntax.Done |. keyword "done"
    , succeed Bind
        |= pattern ()
        |. arrow
        |= lazy expression
    , succeed Do
        |= lazy expression
    ]



-- Atoms -----------------------------------------------------------------------


atom : Parser Atom
atom =
  oneOf
    [ succeed Basic |= basic
    , succeed Variable |= lower
    , succeed Some |. keyword "Some" |= lazy expression
    , succeed None |. keyword "None"
    , succeed List |= list (lazy expression)
    , succeed Record |= dict colon (lazy expression)
    ]


basic : Parser Basic
basic =
  oneOf
    [ succeed (Bool True) |. keyword "True"
    , succeed (Bool False) |. keyword "False"
    , succeed Int |= int
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
  oneOf
    [ symbol "."
        |> andThen (always <| problem "floating point numbers must start with a digit, like 0.25")
    , Parser.float
    ]


string : Parser String
string =
  succeed identity
    |. token "\""
    |= loop [] stringHelper


stringHelper : List String -> Parser (Step (List String) String)
stringHelper revChunks =
  let
    add chunk =
      Loop (chunk :: revChunks)
    isUninteresting char =
      char /= '\\' && char /= '"'
  in
  oneOf
    [ succeed add
        |. token "\\"
        |= oneOf
            [ map (\_ -> "\n") (token "n")
            , map (\_ -> "\t") (token "t")
            , map (\_ -> "\u{000D}") (token "r")
            ]
    , succeed (Parser.Done <| String.join "" <| List.reverse revChunks)
        |. token "\""
    , succeed add
        |= (chompWhile isUninteresting |> getChompedString)
    ]



-- Patterns --------------------------------------------------------------------


pattern : () -> Parser Pattern
pattern () =
  oneOf
    [ succeed PBasic |= basic
    , succeed PVariable |= lower
    , succeed PSome |. keyword "Some" |= lazy pattern
    , succeed PNone |. keyword "None"
    , succeed PCons |= lazy pattern |. doublecolon |= lazy pattern
    , succeed PNil |. doublebracket
    , succeed PRecord |= dict equals (lazy pattern)
    , succeed PIgnore |. underscore
    ]



-- Types -----------------------------------------------------------------------


type_ : () -> Parser Type
type_ () =
  oneOf
    [ succeed TBasic |= basicType
    , succeed TVariable |= universal
    , succeed TOption |. keyword "option" |= lazy type_
    , succeed TList |. keyword "list" |= lazy type_
    , succeed TRecord |= dict colon (lazy type_)
    , succeed TTask |. keyword "task" |= lazy type_
    , succeed TArrow |= lazy type_ |. arrow |= lazy type_
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
