module Helpers.Pretty exposing (brackets, lines, softlines, words)

import Basics.Extra exposing (flip)
import Pretty exposing (..)


a : Doc -> Doc -> Doc
a = flip append


lines : List Doc -> Doc
lines = join line


softlines : List Doc -> Doc
softlines = join softline


words : List Doc -> Doc
words = join space


fold : (a -> Doc -> Doc) -> List a -> Doc
fold f =
  List.foldl f empty


brackets : Doc -> Doc
brackets = surround (char '[') (char ']')
