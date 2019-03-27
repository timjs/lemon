module Language.Lemon.Syntax.Canonical
  ( Decl(..)
  , Error(..)
  , Expr(..)
  , Module(..)
  , Scope
  , canonicalise
  , empty
  ) where


import Basics

import Data.List (List(..))
import Data.Map (Map)
import Language.Lemon.Syntax.Common (Alternative, Atom, Basic(..), Name, Parameter, Pattern(..), Statement, Type(..))

import Language.Lemon.Syntax.Abstract as Abstract



-- DECLARATIONS ----------------------------------------------------------------


type Scope = Map Name Decl


empty :: Scope
empty = Map.empty


data Module
  = Module Scope


data Decl
  = Value Type Expr



-- EXPRESSIONS -----------------------------------------------------------------


data Expr
  = Atom (Atom Expr)
  | Lam Parameter Expr
  | App Expr Expr
  | Let Scope Expr
  | Case Expr (List (Alternative Expr))
  | Seq (List (Statement Expr))


mkIf :: Expr -> Expr -> Expr -> Expr
mkIf test pos neg =
  Case test $ List.fromFoldable
    [ PBasic (Bool true) ** pos
    , PBasic (Bool false) ** neg
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
doScope scope = map doDecl scope # sequence # map Map.fromFoldable


doDecl :: Abstract.Decl -> Either Error (Name ** Decl)
doDecl (Abstract.Value name1 typ name2 params body)
  | name1 == name2 = (name1 ** _) << Value typ <$> doBody typ params body
  | otherwise      = Left $ BadNaming name1 name2


doBody :: Type -> List Pattern -> Abstract.Expr -> Either Error Expr
doBody typ params body = doExpr body >>= go typ params
  where
    go :: Type -> List Pattern -> Expr -> Either Error Expr
    go (TArrow t ts) (Cons p ps) expr = go ts ps $ Lam (p ** t) expr
    go _             (Cons p ps) _    = Left $ BadParameters typ params
    go (TArrow t ts) (Nil)       _    = Left $ BadParameters typ params
    go _             (Nil)       expr = Right $ expr


doExpr :: Abstract.Expr -> Either Error Expr
doExpr = case _ of
  Abstract.Atom atom       -> Atom <$> sequence (map doExpr atom)
  Abstract.Lam params expr -> foldr Lam <$> doExpr expr <*> pure params
  Abstract.App func args   -> foldl App <$> doExpr func <*> sequence (doExpr <$> args)
  Abstract.Let scope expr  -> Let <$> doScope scope <*> doExpr expr
  Abstract.Case test alts  -> Case <$> doExpr test <*> sequence (sequence << map doExpr <$> alts)
  Abstract.If test pos neg -> mkIf <$> doExpr test <*> doExpr pos <*> doExpr neg
  Abstract.Seq stmts       -> Seq <$> sequence (sequence << map doExpr <$> stmts)
