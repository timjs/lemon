module Tophat.Printer where

import Preload
import Data.Doc (Columns(..), Doc, indent, line, lines, list, parens, quotes, record, render, text, words)
import Data.Map (Map, toUnfoldable)
import Data.List (List)
import Tophat.Syntax.Canonical (Alternative, Atom(..), Bindings, Decl(..), Expr(..), Mode(..), Module(..), Name, Pattern(..), Prim(..), PrimType(..), Stmt(..), Type(..))

print :: Module -> String
print = render << doModule

-- DECLARATIONS ----------------------------------------------------------------
doModule :: Module -> Doc
doModule (Module bindings) =
  lines
    [ text preamble
    , line
    , doBindings bindings
    ]

doBindings :: Bindings -> Doc
doBindings bindings = foldlWithIndex doDecl line bindings

doDecl :: Name -> Doc -> Decl -> Doc
doDecl name doc (Value typ expr) =
  lines
    [ doc
    , line
    , words [ text name, text "::", doType typ ]
    , words [ text name, text "=" ]
    , indent $ doExpr expr
    ]

-- EXPRESSIONS -----------------------------------------------------------------
doExpr :: Expr -> Doc
doExpr = case _ of
  Atom a -> doAtom a
  Lam (pat ** typ) inner ->
    parens
      $ words [ text "\\(", doPattern pat, text "::", doType typ, text ") ->", doExpr inner ]
  App func arg -> words [ parens $ doExpr func, parens $ doExpr arg ]
  Let decls inner ->
    lines
      [ text "let"
      , indent $ doBindings decls
      , text "in"
      , doExpr inner
      ]
  Case match alts ->
    lines
      [ words [ text "case", doExpr match, text "of" ]
      , indent $ lines $ map doAlternative alts
      ]
  Seq stmts -> doStmts stmts

-- STATEMENTS ------------------------------------------------------------------
doStmts :: List (Stmt Expr) -> Doc
doStmts stmts =
  lines
    [ text "do"
    , indent $ lines $ map doStmt stmts
    ]

doStmt :: Stmt Expr -> Doc
doStmt = case _ of
  Use pat expr -> words [ text "let", doPattern pat, text "=", doExpr expr ]
  Bind pat expr -> words [ doExpr expr, text ">>=", text "\\", doPattern pat, text "->" ]
  Par mode branches -> using Columns (intercalate sep) $ map (parens << doStmts) branches
    where
    sep =
      wrap $ text
        $ case mode of
            All -> "<&>"
            Any -> "<|>"
  When options ->
    lines
      [ text "only"
      , indent $ lines $ map doWhen options
      ]
  On options ->
    lines
      [ text "only"
      , indent $ lines $ map doOn options
      ]
  --FIXME: How to handle this?
  Done -> words [ text "done" ]
  where
  doWhen option = words [ text "-|", text "when", parens $ doExpr option.guard, doStmts option.body ]

  doOn option = words [ text "-|", text "on", quotes $ text $ option.action, parens $ doExpr option.guard, doStmts option.body ]

-- ATOMS -----------------------------------------------------------------------
doAtom :: Atom Expr -> Doc
doAtom = case _ of
  APrim prim -> doPrim prim
  AVar name -> text name
  AJust inner -> doJust doExpr inner
  ANothing -> doNothing
  AList items -> list $ map doExpr items
  ARecord fields -> record (text ": ") $ map (bimap text doExpr) $ toList fields

doPrim :: Prim -> Doc
doPrim = case _ of
  B true -> text "true"
  B false -> text "false"
  N n -> words [ text "nat", text $ show n ]
  I i -> text $ show i
  R r -> text $ show r
  S s -> quotes $ text s

doCons :: forall a. (a -> Doc) -> a -> a -> Doc
doCons conv head tail = words [ conv head, text ":", conv tail ]

doNil :: Doc
doNil = text "[]"

doJust :: forall a. (a -> Doc) -> a -> Doc
doJust conv inner = words [ text "Just", parens $ conv inner ]

doNothing :: Doc
doNothing = text "Nothing"

-- PATTERNS --------------------------------------------------------------------
doPattern :: Pattern -> Doc
doPattern = case _ of
  PPrim bas -> doPrim bas
  PVar name -> text name
  PJust inner -> doJust doPattern inner
  PNothing -> doNothing
  PCons head tail -> doCons doPattern head tail
  PNil -> doNil
  PRecord fields -> record (text ": ") $ map (bimap text doPattern) $ toList fields
  PIgnore -> text "_"

doAlternative :: Alternative Expr -> Doc
doAlternative (pat ** expr) =
  lines
    [ words [ doPattern pat, text "->" ]
    , indent $ doExpr expr
    ]

-- TYPES -----------------------------------------------------------------------
doType :: Type -> Doc
doType = case _ of
  TPrim b -> doPrimType b
  TVar name -> text name
  TMaybe inner -> words [ text "Maybe", doType inner ]
  TList inner -> words [ text "Array", doType inner ]
  TRecord fields -> record (text ":: ") $ map (bimap text doType) $ toList fields
  TTask inner -> words [ text "Task", doType inner ]
  TArrow left right -> words [ doType left, text "->", doType right ]

doPrimType :: PrimType -> Doc
doPrimType = case _ of
  TBool -> text "Bool"
  TNat -> text "Nat"
  TInt -> text "Int"
  TReal -> text "Real"
  TString -> text "String"

-- PREAMBLE --------------------------------------------------------------------
preamble :: String
preamble =
  """
module Main where

import Preface

-- Maybe some more things ...
"""

-- HELPERS ---------------------------------------------------------------------
toList :: forall k v. Map k v -> List (k ** v)
toList = toUnfoldable
