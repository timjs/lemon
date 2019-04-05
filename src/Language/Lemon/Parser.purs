module Language.Lemon.Parser
  ( module_
  ) where


import Preload hiding (between)
import Language.Lemon.Syntax.Abstract

import Data.List (List(..))
import Text.Parsing.StringParser (Parser, fail, try)
import Text.Parsing.StringParser.CodeUnits (anyChar, anyDigit, anyLetter, char, eof, lowerCaseChar, skipSpaces, string, upperCaseChar)
import Text.Parsing.StringParser.Combinators (between, choice, fix, many, many1, manyTill, option, sepBy)

import Data.Array as Array
import Data.Int as Int
import Data.List.NonEmpty as NonEmpty
import Data.String.CodeUnits as String
import Data.Number as Number



-- MODULES ---------------------------------------------------------------------


module_ :: Parser Module
module_ =
  Module <$ spaces
    <*> scope <* spaces <* eof


scope :: Parser Bindings
scope = fix \self ->
  by semicolon (declaration self)


declaration :: Parser Bindings -> Parser Decl
declaration inner =
  Value
    <$> lower <* colon
    <*> type_ <* semicolon
    <*> lower <* spaces
    <*> by spaces pattern <* equals
    <*> expression inner



-- EXPRESSIONS -----------------------------------------------------------------


expression :: Parser Bindings -> Parser Expr
expression inner = fix \self ->
  choice
    [ Atom
        <$> atom self
    , Lam <$ char '\\' <* spaces
        <*> by spaces parameter <* arrow
        <*> self
    , Let <$ keyword "let" <* spaces
        <*> inner <* spaces <* keyword "in" <* spaces
        <*> self
    , Case <$ keyword "case" <* spaces
        <*> self <* spaces <* keyword "of" <* spaces
        <*> by semicolon (alternative self)
    , If <$ keyword "if" <* spaces
        <*> self <* spaces <* keyword "then" <* spaces
        <*> self <* spaces <* keyword "else" <* spaces
        <*> self
    , Seq <$ keyword "do" <* spaces
        <*> by semicolon (statement self)
    , parens self
    ]
    >>= maybeCall self
  where
    maybeCall self expr =
      choice
        [ App expr <$ try spaces <*> by spaces self
        , pure expr
        ]


parameter :: Parser Parameter
parameter =
  (**)
    <$> pattern <* colon
    <*> type_


alternative :: Parser Expr -> Parser (Alternative Expr)
alternative inner =
  (**)
    <$> pattern <* arrow
    <*> inner


statement :: Parser Expr -> Parser (Stmt Expr)
statement inner =
  choice
    [ Set <$ keyword "let" <* spaces
        <*> pattern <* equals
        <*> inner
    , (\x xs -> Par All (Cons x xs)) <$ keyword "do" <* spaces
        <*> by semicolon (statement inner)
        <*> by spaces (keyword "also" *> spaces *> by semicolon (statement inner))
    , map On $
        by spaces $
          { action: _, guard: _, body: _ } <$ keyword "on" <* spaces
            <*> doublequoted <* spaces <* keyword "when" <* spaces
            <*> inner <* spaces <* keyword "do" <* spaces
            <*> by semicolon (statement inner)
    , map When $
        by spaces $
          { guard: _, body: _ } <$ keyword "when" <* spaces
            <*> inner <* spaces <* keyword "do" <* spaces
            <*> by semicolon (statement inner)
    , Done <$ keyword "done"
    , Bind
        <$> pattern <* arrow
        <*> inner
    ]


-- ATOMS -----------------------------------------------------------------------


atom :: Parser Expr -> Parser (Atom Expr)
atom inner =
  choice
    [ APrim <$> basic
    , AVar <$> lower
    , AJust <$ keyword "Just" <* spaces <*> inner
    , ANothing <$ keyword "Nothing"
    , AList <$> list inner
    , ARecord <$> record colon inner
    ]


basic :: Parser Prim
basic =
  choice
    [ B true <$ keyword "True"
    , B false <$ keyword "False"
    , I <$> try int --NOTE: ints are like floats, we need to backtrack here
    , F <$> float
    , S <$> doublequoted
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


--FIXME: Does this eat the '"' or not?
doublequoted :: Parser String
doublequoted = fromChars <$ char '"' <*> manyTill anyChar (char '"')


list :: forall a. Parser a -> Parser (List a)
list item = between (char '[' <* spaces) (spaces *> char ']') $ by comma item


record :: forall a. Parser Unit -> Parser a -> Parser (Fields a)
record sep item = between (char '{' <* spaces) (spaces *> char '}') $ by comma entry
  where
    entry = { name: _, value: _ } <$> lower <* spaces <* sep <*> item



-- PATTERNS --------------------------------------------------------------------


pattern :: Parser Pattern
pattern = fix \self ->
  choice
    [ PPrim <$> basic
    , PVar <$> lower
    , PJust <$ keyword "Just" <* spaces <*> self
    , PNothing <$ keyword "Nothing"
    , PNil <$ string "[]"
    , PRecord <$> record equals self
    , PIgnore <$ char '_'
    , parens self
    ]
    >>= maybeCons self
  where
    maybeCons self left =
      choice
        [ PCons left <$ try doublecolon <*> self
        , pure left
        ]



-- TYPES -----------------------------------------------------------------------


type_ :: Parser Type
type_ = fix \self ->
  choice
    [ TPrim <$> basicType
    , TVar <$> universal
    , TMaybe <$ keyword "option" <* spaces <*> self
    , TList <$ keyword "list" <* spaces <*> self
    , TRecord <$> record colon self
    , TTask <$ keyword "task" <* spaces <*> self
    , parens self
    ]
    >>= maybeArrow self
  where
    maybeArrow self left =
      choice
        [ TArrow left <$ try arrow <*> self
        , pure left
        ]


basicType :: Parser PrimType
basicType =
  choice
    [ TBool <$ keyword "bool"
    , TInt <$ keyword "int"
    , TFloat <$ keyword "float"
    , TString <$ keyword "string"
    ]



-- HELPERS ---------------------------------------------------------------------


by :: forall s a. Parser s -> Parser a -> Parser (List a)
by = flip sepBy


parens :: forall a. Parser a -> Parser a
parens = between (char '(') (char ')')


keyword :: String -> Parser String
keyword = string


fromChars :: forall f. Foldable f => f Char -> String
fromChars = String.fromCharArray << Array.fromFoldable


anyNameChar :: Parser Char
anyNameChar = anyLetter <|> char '_'



-- SPACES --


spaces :: Parser Unit
spaces = skipSpaces


-- SYMBOLS --


spacy :: forall a. Parser a -> Parser Unit
spacy item = void $ spaces <* item <* spaces


comma :: Parser Unit
comma = spacy $ char ','


semicolon :: Parser Unit
semicolon = spacy $ char ';'

colon :: Parser Unit
colon = spacy $ char ':'


equals :: Parser Unit
equals = spacy $ char '='


arrow :: Parser Unit
arrow = spacy $ string "->"


doublecolon :: Parser Unit
doublecolon = spacy $ string "::"



-- NAMES --


-- keywords :: Set String
-- keywords =
--   Set.fromFoldable
--     [ "type"
--     , "let"
--     , "in"
--     , "case"
--     , "of"
--     , "if"
--     , "then"
--     , "else"
--     , "do"
--     , "also"
--     , "on"
--     , "when"
--     , "done"
--     ]


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
