module Program.Scratch exposing (main)

-- Parsed --


handleParsed : Lemon.Module -> Html msg
handleParsed mod =
  div []
    [ text <| Debug.toString mod
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


handleError : Parser.Error -> Html msg
handleError { row, col, source, problem, context } =
  let
    firstContext =
      case List.head context of
        Just ctx ->
          ctx.description
        Nothing -> "toplevel"
  in
  div []
    [ p [] [ text <| "I found a problem while parsing this " ++ firstContext ++ ":" ]
    , handleSource row col source
    , p [] [ text <| "I am looking for one of the following things:" ]
    , handleProblem problem
    ]


handleSource : Int -> Int -> String -> Html msg
handleSource row col source =
  let
    handleMaybe maybe =
      case maybe of
        Just line ->
          line
        Nothing -> ""
    line =
      source
        |> String.lines
        --NOTE: indexing starts at 0, rows at 1
        |> List.getAt (row - 1)
        |> handleMaybe
    num =
      toString row
        ++ "| "
    carret =
      --NOTE: cols start at 1 and need to add lenght of number decorator
      String.repeat (col - 1 + String.length num) " " ++ "^"
  in
  pre []
    [ text <| num ++ line
    , br [] []
    , text <| carret
    ]


handleProblem : Problem -> Html msg
handleProblem problem =
  let
    bullet line =
      li [] [ line ]
  in
  case problem of
    BadOneOf problems ->
      p []
        [ text "I am looking for one of the following things:"
        , ul [] (List.map (bullet << handleProblem) problems)
        ]
    BadInt -> p [] [ text "an integer" ]
    BadFloat -> p [] [ text "a float" ]
    BadRepeat -> p [] [ text "whitespace" ]
    ExpectingEnd -> p [] [ text "end of the string" ]
    ExpectingSymbol symbol ->
      p [] [ text <| "the symbol `" ++ symbol ++ "`" ]
    ExpectingKeyword keyword ->
      p [] [ text <| "the keyword `" ++ keyword ++ "`" ]
    ExpectingVariable -> p [] [ text <| "a variable" ]
    ExpectingClosing closing ->
      p [] [ text <| "a closing `" ++ closing ++ "`" ]
    Fail message ->
      p [] [ text message ]
