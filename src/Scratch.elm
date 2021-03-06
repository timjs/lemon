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



--
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
    Parser.Expecting string ->
      "expecting " ++ string
    Parser.ExpectingInt -> "a decimal integer"
    Parser.ExpectingHex -> "a hexadecimal integer"
    Parser.ExpectingOctal -> "an octal integer"
    Parser.ExpectingBinary -> "a binary integer"
    Parser.ExpectingFloat -> "a float"
    Parser.ExpectingNumber -> "a number"
    Parser.ExpectingVariable -> "a variable"
    Parser.ExpectingSymbol symbol ->
      "the symbol `" ++ symbol ++ "`"
    Parser.ExpectingKeyword keyword ->
      "the keyword `" ++ keyword ++ "`"
    Parser.ExpectingEnd -> "end of the string"
    Parser.UnexpectedChar -> "something I didn't expect"
    Parser.Problem string ->
      string
    Parser.BadRepeat -> "whitespace"



--


stringHelper : List String -> Parser (Step (List String) String)
stringHelper revChunks =
  let
    add chunk =
      Loop (chunk :: revChunks)
    isUninteresting char =
      char /= '\\' && char /= '"'
  in
  oneOf
    [ succeed add
        |. token "\\"
        |= oneOf
            [ map (\_ -> "\n") (token "n")
            , map (\_ -> "\t") (token "t")
            , map (\_ -> "\u{000D}") (token "r")
            ]
    , succeed (Parser.Done <| String.join "" <| List.reverse revChunks)
        |. token "\""
    , succeed add
        |= (chompWhile isUninteresting |> getChompedString)
    ]
