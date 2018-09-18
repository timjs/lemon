module Helpers.Pretty exposing (brackets, lines, softlines, words)

import Basics.Extra exposing (flip)
import Pretty exposing (..)


{-| Short hand notation for append.
Usefull when appending multiple parts together:

    string "Hello"
      |> a space
      |> a "World"
      |> a (char '!')
      |> a line

-}
a : Doc -> Doc -> Doc
a = flip append


{-| Concatenate a list of documents together interspersed with lines.
Very convenient when laying out lines after another:

    lines
      [ string "Heading"
      , empty
      , words [string "First", string "paragraph"]
      ...
      ]

    ==

    string "Heading"
      |> a line
      |> a line
      |> a (string "First")
      |> a space
      |> a (string "paragraph")
      ...

See also `words`.

-}
lines : List Doc -> Doc
lines = join line


{-| Like `lines` but uses `softline` instaed.
-}
softlines : List Doc -> Doc
softlines = join softline


{-| Concatenate a list of documents together interspersed with spaces.
Very convenient when laying out words after another.

See also `lines`.

-}
words : List Doc -> Doc
words = join space


{-| Fold a list of documents from left to right using a given function.

    fold f == List.foldl f empty

-}
fold : (a -> Doc -> Doc) -> List a -> Doc
fold f =
  List.foldl f empty


{-| Wraps a document in brackets.
-}
brackets : Doc -> Doc
brackets = surround (char '[') (char ']')
