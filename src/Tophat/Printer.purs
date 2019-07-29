module Tophat.Printer where
{-
import Preload
import Data.Array as Array
import Tophat.Syntax.Canonical
import Text.Pretty (Doc, hcat, vcat, text)
import Text.Pretty as Pretty

print :: Module -> String
print = Pretty.render << doModule

-- DECLARATIONS ----------------------------------------------------------------
doModule :: Module -> Doc
doModule (Module bs) =
  vcat
    [ text preamble
    , neutral
    , doBindings bs
    ]

doBindings :: Bindings -> Doc
doBindings = foldl doDecl neutral

doDecl :: Name -> Decl -> Doc -> Doc
doDecl name (Value typ expr) doc =
  vcat
    [ hsep [ text name, text "::", doType typ ]
    , hsep [ text name, text "=" ]
    , indent $ doExpr expr
    , neutral
    ]

-- EXPRESSIONS -----------------------------------------------------------------
doExpr :: Expr -> Doc
doExpr = case _ of
  Atom a -> doAtom a
  Lam (pat ** _) inner -> parens $ hsep [ text "\\", doPattern pat, text "->", doExpr inner ]
  App func arg -> hsep [ parens $ doExpr func, parens $ doExpr arg ]
  Let decls inner ->
    vcat
      [ text "let"
      , indent $ doBindings decls
      , text "in"
      , doExpr inner
      ]
  Case match alts ->
    vcat
      [ hsep [ text "case", doExpr match, text "of" ]
      , indent $ foldl doAlternative neutral alts
      ]
  Seq stmts -> parens $ vcat $ map doStmt stmts

-- STATEMENTS ------------------------------------------------------------------
doStmt :: Stmt Expr -> Doc
doStmt = case _ of
  Set pat expr -> hsep [ text "let", doPattern pat, text "=", doExpr expr, text "in" ]
  Bind pat expr -> bind pat expr
  Seq expr -> bind PIgnore expr
  Par brns -> vcat $ map par brns
  When brns ->
    vcat
      [ text ">>>"
      , indent $ list $ map pair2 brns
      ]
  On brns ->
    vcat
      [ text ">?>"
      , indent $ list $ map pair3 brns
      ]
  --FIXME: How to handle this?
  Done -> hsep [ text "return", tuple $ map text $ some_tuple_with_visible_names ]
  where
  bind pat expr = hsep [ doExpr expr, text ">>=", text "\\", doPattern pat, text "->" ]

  pair2 (pred ** stmts) = tuple [ doExpr pred, doStmt ?stmt ]

  pair3 (name ** pred ** stmts) = tuple [ text name, doExpr pred, doStmt ?stmt ]

  par stmts =
    vcat
      [ text "also"
      , indent $ vcat $ map doStmt stmts
      ]

  some_tuple_with_visible_names = undefined

-- ATOMS -----------------------------------------------------------------------
doAtom :: Atom Expr -> Doc
doAtom = case _ of
  APrim bas -> doPrim bas
  AVar name -> text name
  AJust inner -> just doExpr inner
  ANothing -> nothing
  AList xs -> ?alist
  ARecord fields -> tuple $ map (doExpr << snd) fields

doPrim :: Prim -> Doc
doPrim = case _ of
  B true -> text "True"
  B false -> text "False"
  I i -> text $ show i
  F f -> text $ show f
  S s -> surround (text "\"") (text "\"") $ text s

cons conv head tail = brackets $ hsep [ conv head, text ":", conv tail ]

nil = text "[]"

just conv inner = hsep [ text "Just", parens $ conv inner ]

nothing = text "Nothing"

-- PATTERNS --------------------------------------------------------------------
doPattern :: Pattern -> Doc
doPattern = case _ of
  PPrim bas -> doPrim bas
  PVar name -> text name
  PJust inner -> just doPattern inner
  PNothing -> nothing
  PCons head tail -> cons doPattern head tail
  PNil -> nil
  PRecord fields -> tuple $ map (text << fst) fields
  PIgnore -> text "_"

doAlternative :: Alternative Expr -> Doc -> Doc
doAlternative (pat ** expr) doc =
  vcat
    [ hsep [ doPattern pat, text "->" ]
    , indent $ doExpr expr
    ]

-- TYPES -----------------------------------------------------------------------
doType :: Type -> Doc
doType = case _ of
  TPrim b -> doPrimType b
  TVar name -> text name
  TMaybe inner -> hsep [ text "Maybe", doType inner ]
  TList inner -> hsep [ text "Array", doType inner ]
  TRecord fields -> tuple $ map (doType << snd) fields
  TTask inner -> hsep [ text "Task", doType inner ]
  TArrow left right -> hsep [ parens $ doType left, text "->", doType right ]

doPrimType :: PrimType -> Doc
doPrimType b = case b of
  TBool -> text "Bool"
  TInt -> text "Int"
  TFloat -> text "Real"
  TString -> text "String"

-- HELPERS ---------------------------------------------------------------------
hsep :: forall f. Foldable f => f Doc -> Doc
hsep = intercalate space

indent :: Doc -> Doc
indent d = hcat [ Pretty.empty 2 0, d ]

enclose :: Doc -> Doc -> Doc -> Doc
enclose l r d = hcat [ l, d, r ]

parens :: Doc -> Doc
parens = enclose (text "(") (text ")")

brackets :: Doc -> Doc
brackets = enclose (text "[") (text "]")

tuple :: Foldable f => f Doc -> Doc
tuple = parens << hcat (text ",")

list :: Foldable f => f Doc -> Doc
list = brackets << hcat (text ",")

space :: Doc
space = Pretty.empty 1 0

-- PREAMBLE --------------------------------------------------------------------
preamble :: String
preamble =
  """
module Main where

import Preface

...some more things...
"""
-}

-- hcat <=> join
-- hsep <=> words
-- vcat <=> lines
