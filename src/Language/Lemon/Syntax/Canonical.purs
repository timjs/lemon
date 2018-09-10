module Language.Lemon.Syntax.Canonical
  ( Declaration(..)
  , Error(..)
  , Expression(..)
  , Module(..)
  , Scope
  , canonicalise
  , empty
  ) where


import Basics

import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.List as List

import Language.Lemon.Syntax.Common
import Language.Lemon.Syntax.Abstract as Abstract



-- DATA ------------------------------------------------------------------------

-- DECLARATIONS --


type Scope = Map Name Declaration


empty :: Scope
empty = Map.empty


data Module
  = Module Scope


data Declaration
  = Value Type Expression



-- EXPRESSIONS --


data Expression
  = Atom (Atom Expression)
  | Lambda Parameter Expression
  | Call Expression Expression
  | Let Scope Expression
  | Case Expression (List (Alternative Expression))
  | Sequence (List (Statement Expression))


mkIf :: Expression -> Expression -> Expression -> Expression
mkIf test pos neg =
  Case test $ List.fromFoldable
    [ PBasic (Bool true): pos
    , PBasic (Bool false): neg
    ]


-- CANONICALISE ----------------------------------------------------------------


data Error
  -- = Redefinition Name Name
  -- | Duplication Name
  = BadParameters Type (List Pattern)
  | BadNaming Name Name


canonicalise :: Abstract.Module -> Either Error Module
canonicalise (Abstract.Module scope) = Module <$> doScope scope


--XXX: why is eta-expansion needed here???
doScope :: Abstract.Scope -> Either Error Scope
doScope scope = map doDeclaration scope # sequence # map Map.fromFoldable


doDeclaration :: Abstract.Declaration -> Either Error (Tuple Name Declaration)
doDeclaration (Abstract.Value name1 typ name2 params body)
  | name1 == name2 = Tuple name1 << Value typ <$> doBody typ params body
  | otherwise      = Left $ BadNaming name1 name2


doBody :: Type -> List Pattern -> Abstract.Expression -> Either Error Expression
doBody typ params body = doExpression body >>= go typ params
  where
    go :: Type -> List Pattern -> Expression -> Either Error Expression
    go (TArrow t ts) (Cons p ps) expr = go ts ps $ Lambda (p : t) expr
    go _             (Cons p ps) _    = Left $ BadParameters typ params
    go (TArrow t ts) (Nil)       _    = Left $ BadParameters typ params
    go _             (Nil)       expr = Right $ expr


doExpression :: Abstract.Expression -> Either Error Expression
doExpression (Abstract.Atom atom)          = Atom <$> sequence (map doExpression atom)
doExpression (Abstract.Lambda params expr) = foldr Lambda <$> doExpression expr <*> pure params
doExpression (Abstract.Call func args)     = foldl Call <$> doExpression func <*> sequence (doExpression <$> args)
doExpression (Abstract.Let scope expr)     = Let <$> doScope scope <*> doExpression expr
doExpression (Abstract.Case test alts)     = Case <$> doExpression test <*> sequence (sequence << map doExpression <$> alts)
doExpression (Abstract.If test pos neg)    = mkIf <$> doExpression test <*> doExpression pos <*> doExpression neg
doExpression (Abstract.Sequence stmts)     = Sequence <$> sequence (sequence <$> map doExpression <$> stmts)
