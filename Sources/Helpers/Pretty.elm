module Helpers.Pretty exposing (a, brackets, lines, softlines, words)

import Helpers.Function exposing (flip)
import Pretty exposing (Doc, append, char, join, line, softline, space, surround)


a : Doc -> Doc -> Doc
a = flip append


lines : List Doc -> Doc
lines = join line


softlines : List Doc -> Doc
softlines = join softline


words : List Doc -> Doc
words = join space


brackets : Doc -> Doc
brackets = surround (char '[') (char ']')
