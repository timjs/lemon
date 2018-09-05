module Program.Parser exposing
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
import Lemon.Parser as Lemon
import Lemon.Syntax as Lemon
import Parser



-- Model -----------------------------------------------------------------------


type alias Model =
  { input : String
  , output : Result (List Parser.DeadEnd) Lemon.Module
  }


example : String
example = "id : 'a -> 'a; id = \\x -> x"


init : Model
init =
  { input = example
  , output = Ok (Lemon.Module Lemon.empty)
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
        , output = Lemon.parse string
      }



-- View ------------------------------------------------------------------------


view : Model -> Html Msg
view { input, output } =
  let
    parsing =
      case output of
        Ok mod ->
          Debug.toString mod
        Err errors ->
          Parser.deadEndsToString errors
  in
  form []
    [ textarea
        [ Html.rows 12
        , Html.value input
        , Html.onInput Enter
        ]
        []
    , text parsing
    ]



-- Main ------------------------------------------------------------------------


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
