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

import Data.Map (Map)
import Data.Map as Map
import Data.List (List)

import Language.Lemon.Syntax.Abstract as Abstract
import Language.Lemon.Syntax.Common



-- DATA ------------------------------------------------------------------------

-- DECLARATIONS --


type Scope =
  Map Name Declaration

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



-- CANONICALISE ----------------------------------------------------------------


data Error
  = Redefinition Name Name
  | Duplication Name
  | Disagreement Name Name


canonicalise :: Abstract.Module -> Either Error Module
canonicalise (Abstract.Module scope) = Module <$> doScope scope


--XXX: why is eta-expansion needed here???
doScope :: Abstract.Scope -> Either Error Scope
doScope scope = map doDeclaration scope # sequence # map Map.fromFoldable


doDeclaration :: Abstract.Declaration -> Either Error (Tuple Name Declaration)
doDeclaration (Abstract.Value name1 typ name2 params body)
  | name1 == name2 = Tuple name1 << Value typ <$> doBody typ params body
  | otherwise      = Left $ Disagreement name1 name2


doBody :: Type -> List Pattern -> Abstract.Expression -> Either Error Expression
doBody typ params body =
  --FIXME: eta-reduct parameters
  doExpression body


doExpression :: Abstract.Expression -> Either Error Expression
doExpression (Abstract.Atom atom)          = Atom <$> sequence (map doExpression atom)
doExpression (Abstract.Lambda params expr) = foldr Lambda <$> doExpression expr <*> pure params
doExpression (Abstract.Call func args)     = foldl Call <$> doExpression func <*> sequence (map doExpression args)
doExpression (Abstract.Let scope expr)     = Let <$> doScope scope <*> doExpression expr
doExpression (Abstract.Case test alts)     = Case <$> doExpression test <*> sequence (map (sequence << map doExpression) alts)
doExpression (Abstract.If test pos neg)    = ?hole
doExpression (Abstract.Sequence stmts)     = ?hole
