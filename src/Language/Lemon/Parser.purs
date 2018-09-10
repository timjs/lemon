module Language.Lemon.Parser
  ( toplevel
  ) where


import Basics hiding (between)

import Control.Lazy (defer)

import Data.Array as Array
import Data.Char.Unicode as Char
import Data.Int as Int
import Data.List (List(..))
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits as String
import Data.Number as Number

import Text.Parsing.StringParser
import Text.Parsing.StringParser.String
import Text.Parsing.StringParser.Combinators

import Language.Lemon.Name (Name)
import Language.Lemon.Syntax.Abstract
import Language.Lemon.Syntax.Common

toplevel = module_


-- MODULES ---------------------------------------------------------------------


module_ :: Parser Module
module_ =
  Module <$ spaces
    <*> scope <* spaces <* eof


scope :: Parser Scope
scope = by semicolon declaration


declaration :: Parser Declaration
declaration =
  Value
    <$> lower <* colon
    <*> type_ <* semicolon
    <*> lower <* spaces
    <*> by spaces pattern <* equals
    <*> expression



-- EXPRESSIONS -----------------------------------------------------------------


expression :: Parser Expression
expression =
  choice
    [ Atom
        <$> atom
    , Lambda <$ char '\\' <* spaces
        <*> by spaces parameter <* arrow
        <*> lazy (\_ -> expression)
    , Let <$ keyword "let" <* spaces
        <*> lazy (\_ -> scope) <* spaces <* keyword "in" <* spaces
        <*> lazy (\_ -> expression)
    , Case <$ keyword "case" <* spaces
        <*> lazy (\_ -> expression) <* spaces <* keyword "of" <* spaces
        <*> by semicolon alternative
    , If <$ keyword "if" <* spaces
        <*> lazy (\_ -> expression) <* spaces <* keyword "then" <* spaces
        <*> lazy (\_ -> expression) <* spaces <* keyword "else" <* spaces
        <*> lazy (\_ -> expression)
    , Sequence <$ keyword "do" <* spaces
        <*> by semicolon statement
    , parens (lazy (\_ -> expression))
    ]
    >>= maybeCall
  where
    maybeCall expr =
      choice
        [ Call expr <$ try spaces <*> by spaces expression
        , pure expr
        ]


parameter :: Parser Parameter
parameter =
  Tuple
    <$> pattern <* colon
    <*> type_


alternative :: Parser (Alternative Expression)
alternative =
  Tuple
    <$> pattern <* arrow
    <*> lazy (\_ -> expression)


statement :: Parser (Statement Expression)
statement =
  choice
    [ Set <$ keyword "let" <* spaces
        <*> pattern <* equals
        <*> lazy (\_ -> expression)
    , (\x xs -> Par (Cons x xs)) <$ keyword "do" <* spaces
        <*> by semicolon (lazy (\_ -> statement))
        <*> by spaces (keyword "also" *> spaces *> by semicolon (lazy (\_ -> statement)))
    , map On $
        by spaces $
          { action: _, predicate: _, body: _ } <$ keyword "on" <* spaces
            <*> doublequotedString <* spaces <* keyword "when" <* spaces
            <*> lazy (\_ -> expression) <* spaces <* keyword "do" <* spaces
            <*> by semicolon (lazy (\_ -> statement))
    , map When $
        by spaces $
          { predicate: _, body: _ } <$ keyword "when" <* spaces
            <*> lazy (\_ -> expression) <* spaces <* keyword "do" <* spaces
            <*> by semicolon (lazy (\_ -> statement))
    , Done <$ keyword "done"
    , Bind
        <$> pattern <* arrow
        <*> lazy (\_ -> expression)
    , Do
        <$> lazy (\_ -> expression)
    ]


-- ATOMS -----------------------------------------------------------------------


atom :: Parser (Atom Expression)
atom =
  choice
    [ Basic <$> basic
    , Variable <$> lower
    , Some <$ keyword "Some" <* spaces <*> lazy (\_ -> expression)
    , None <$ keyword "None"
    , List <$> list (lazy (\_ -> expression))
    , Record <$> record colon (lazy (\_ -> expression))
    ]


basic :: Parser Basic
basic =
  choice
    [ Bool true <$ keyword "True"
    , Bool false <$ keyword "False"
    , Int <$> try int --NOTE: ints are like floats, we need to backtrack here
    , Float <$> float
    , String <$> doublequotedString
    ]


int :: Parser Int
int = negate <$ char '-' <*> int' <|> int'
  where
    int' :: Parser Int
    int' = do
      r <- many1 anyDigit
      case Int.fromString $ fromChars r of
        Nothing -> fail "expecting integer"
        Just i -> pure i


float :: Parser Number
float = negate <$ char '-' <*> float' <|> float'
  where
    float' :: Parser Number
    float' = do
      i <- many1 anyDigit
      j <- option (NonEmpty.singleton '0') (char '.' *> many1 anyDigit)
      case Number.fromString $ fromChars $ i <> j of
        Nothing -> fail "expecting float"
        Just n -> pure n


doublequotedString :: Parser String
doublequotedString = fromChars <$ char '"' <*> manyTill anyChar (char '"')


list :: forall a. Parser a -> Parser (List a)
list item = between (char '[' <* spaces) (spaces *> char ']') $ by comma item


record :: forall a. Parser Unit -> Parser a -> Parser (Fields a)
record sep item = between (char '{' <* spaces) (spaces *> char '}') $ by comma entry
  where
    entry = Tuple <$> lower <* spaces <* sep <*> item



-- PATTERNS --------------------------------------------------------------------


pattern :: Parser Pattern
pattern =
  choice
    [ PBasic <$> basic
    , PVariable <$> lower
    , PSome <$ keyword "Some" <* spaces <*> lazy (\_ -> pattern)
    , PNone <$ keyword "None"
    , PNil <$ string "[]"
    , PRecord <$> record equals (lazy (\_ -> pattern))
    , PIgnore <$ char '_'
    , parens (lazy (\_ -> pattern))
    ]
    >>= maybeCons
  where
    maybeCons left =
      choice
        [ PCons left <$ try doublecolon <*> pattern
        , pure left
        ]



-- TYPES -----------------------------------------------------------------------


type_ :: Parser Type
type_ =
  choice
    [ TBasic <$> basicType
    , TVariable <$> universal
    , TOption <$ keyword "option" <* spaces <*> lazy (\_ -> type_)
    , TList <$ keyword "list" <* spaces <*> lazy (\_ -> type_)
    , TRecord <$> record colon (lazy (\_ -> type_))
    , TTask <$ keyword "task" <* spaces <*> lazy (\_ -> type_)
    , identity <$> parens (lazy (\_ -> type_))
    ]
    >>= maybeArrow
  where
    maybeArrow left =
      choice
        [ TArrow left <$ try arrow <*> type_
        , pure left
        ]


basicType :: Parser BasicType
basicType =
  choice
    [ TBool <$ keyword "bool"
    , TInt <$ keyword "int"
    , TFloat <$ keyword "float"
    , TString <$ keyword "string"
    ]



-- HELPERS ---------------------------------------------------------------------


-- SPACES --


spaces = skipSpaces



-- SYMBOLS --


spacy :: forall a. Parser a -> Parser Unit
spacy item = void $ spaces <* item <* spaces

comma      = spacy $ char ','
semicolon  = spacy $ char ';'
colon      = spacy $ char ':'
equals     = spacy $ char '='

arrow       = spacy $ string "->"
doublecolon = spacy $ string "::"



-- NAMES --


keywords :: Set String
keywords =
  Set.fromFoldable
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


--FIXME: check for keywords?
name :: Parser Char -> Parser Name
name first = conv <$> first <*> many anyNameChar
  where
    conv c cs = fromChars $ Cons c cs


lower :: Parser Name
lower = name lowerCaseChar


upper :: Parser Name
upper = name upperCaseChar


universal :: Parser Name
universal = char '\'' *> lower



-- TODO --

by = flip sepBy

variable = string
keyword = string
symbol = string
token = string

lazy = defer

parens = between (char '(') (char ')')



-- NEW HELPERS --


fromChars = String.fromCharArray << Array.fromFoldable


anyNameChar = anyLetter <|> char '_'
