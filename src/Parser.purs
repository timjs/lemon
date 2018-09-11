module Program.Parser
  where

{-


import Html exposing (..)
import Html.Attributes as Html
import Html.Events as Html
import Lemon.Parser exposing (parse)
import Lemon.Syntax.Abstract exposing (Module(..), empty)
import List.Extra as List
import Parser



-- Model -----------------------------------------------------------------------


type alias Model =
  { input : String
  , output : Result (List Parser.DeadEnd) Module
  }


example : String
example = """
map : ('a -> 'b) -> list 'a -> list 'b;
map f list = case list of
  [] -> [];
  x :: xs -> cons (f x) (map f xs)
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
        Ok mod ->
          text <| Debug.toString mod
        Err errors ->
          handleErrors input errors
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



-- Main ------------------------------------------------------------------------


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }

    
-}
