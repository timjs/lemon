module Program.Parse exposing
  ( Model
  , Msg
  , init
  , update
  , view
  )

import Browser
import Dict
import Html exposing (..)
import Html.Attributes as Html
import Html.Events as Html
import Lemon.Parse exposing (parse)
import Lemon.Syntax.Textual exposing (Module(..), empty)
import List.Extra as List
import Parser



-- Model -----------------------------------------------------------------------


type alias Model =
  { input : String
  , output : Result (List Parser.DeadEnd) Module
  }


example : String
example = """
map : (a -> b) -> List a -> List b;
map f list = case list of
  [] -> [];
  x :: xs -> f x :: map f xs
  """


init : Model
init =
  { input = example
  , output = Ok (Module empty)
  }



-- Updates ---------------------------------------------------------------------


type Msg
  = Enter String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Enter string ->
      { model
        | input = string
        , output = parse string
      }



-- View ------------------------------------------------------------------------


view : Model -> Html Msg
view { input, output } =
  let
    parsing =
      case output of
        Ok mod -> text <| Debug.toString mod
        Err errors -> handleErrors input errors
  in
  form []
    [ textarea
        [ Html.rows 12
        , Html.value input
        , Html.onInput Enter
        ]
        []
    , br [] []
    , parsing
    ]



-- Errors --
{-
   -- SYNTAX PROBLEM ---------------------------------------- ./src/Lemon/Parser.elm

   I need whitespace, but got stuck on what looks like a new declaration. You are
   either missing some stuff in the declaration above or just need to add some
   spaces here:

   115| parallel =
        ^
   I am looking for one of the following things:

       whitespace

   Detected errors in 1 module.
-}


handleErrors : String -> List Parser.DeadEnd -> Html msg
handleErrors source errors =
  let
    enumerate this =
      li [] [ this ]
  in
  div []
    [ p [] [ text <| "I found a problem while parsing the input." ]
    , p [] [ text <| "I am looking for one of the following things:" ]
    , ul [] <| List.map (enumerate << handleError source) errors
    ]


handleError : String -> Parser.DeadEnd -> Html msg
handleError source { row, col, problem } =
  div []
    [ handleSource source row col
    , handleProblem problem
    ]


handleSource : String -> Int -> Int -> Html msg
handleSource source row col =
  let
    line =
      source
        |> String.lines
        --NOTE: indexing starts at 0, rows at 1
        |> List.getAt (row - 1)
        |> Maybe.withDefault ""
    num =
      String.fromInt row
        ++ "| "
    carret =
      --NOTE: cols start at 1 and need to add length of number decorator
      String.repeat (col - 1 + String.length num) " " ++ "^"
  in
  pre []
    [ text <| num ++ line
    , br [] []
    , text <| carret
    ]


handleProblem : Parser.Problem -> Html msg
handleProblem problem =
  p [] [ text (showProblem problem) ]


showProblem : Parser.Problem -> String
showProblem problem =
  case problem of
    Parser.Expecting string -> "expecting " ++ string
    Parser.ExpectingInt -> "a decimal integer"
    Parser.ExpectingHex -> "a hexadecimal integer"
    Parser.ExpectingOctal -> "an octal integer"
    Parser.ExpectingBinary -> "a binary integer"
    Parser.ExpectingFloat -> "a float"
    Parser.ExpectingNumber -> "a number"
    Parser.ExpectingVariable -> "a variable"
    Parser.ExpectingSymbol symbol -> "the symbol `" ++ symbol ++ "`"
    Parser.ExpectingKeyword keyword -> "the keyword `" ++ keyword ++ "`"
    Parser.ExpectingEnd -> "end of the string"
    Parser.UnexpectedChar -> "something I didn't expect"
    Parser.Problem string -> string
    Parser.BadRepeat -> "whitespace"



-- Main ------------------------------------------------------------------------


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
