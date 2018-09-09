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



-- Modules and Definitions -----------------------------------------------------


data Module
  = Module Scope


type Scope =
  Map Name Declaration


data Declaration
  = Value Type Expression



-- Expressions -----------------------------------------------------------------


data Expression
  = Atom (Atom Expression)
  | Lambda Parameter Expression
  | Call Expression Expression
  | Let Scope Expression
  | Case Expression (List (Alternative Expression))
  | Sequence (List (Statement Expression))



-- Init ------------------------------------------------------------------------


empty :: Scope
empty = Map.empty



-- Canonicalise ----------------------------------------------------------------


data Error
  = Redefinition Name Name
  | Duplication Name
  | Disagreement Name Name


canonicalise :: Abstract.Module -> Either Error Module
canonicalise (Abstract.Module scope) = Module <$> doScope scope


doScope :: Abstract.Scope -> Either Error Scope
doScope = map doDeclaration >> sequence >> map Map.fromFoldable


doDeclaration :: Abstract.Declaration -> Either Error { name :: Name, declaration :: Declaration }
doDeclaration (Abstract.Value name1 annot name2 params body)
  | name1 == name2 = create <$> doBody annot params body
    where
      create res = { name: name1, declaration: Value annot res }
  | otherwise      = Left $ Disagreement name1 name2


doBody :: Type -> List Pattern -> Abstract.Expression -> Either Error Expression
doBody annot params body =
  --FIXME: eta-reduct parameters
  doExpression body


doExpression :: Abstract.Expression -> Either Error Expression
doExpression (Abstract.Atom atom)          = Atom <$> sequence (map doExpression atom)
doExpression (Abstract.Lambda locals body) = ?hole
doExpression (Abstract.Call func args)     = ?hole
doExpression (Abstract.Let scope body)     = ?hole
doExpression (Abstract.Case test alts)     = ?hole
doExpression (Abstract.If test pos neg)    = ?hole
doExpression (Abstract.Sequence stmts)     = ?hole
