module TwoPlayerMatch exposing 
  ( TwoPlayerMatch
  , sampleData
  , matchTypeToString
  )

import Time
import Parser exposing (Parser, (|.), (|=))
import Iso8601

type alias TwoPlayerMatch =
 { player1Name : String
 , player1Score : Int
 , player2Score : Int
 , player2Name : String
 , event : String
 , edition : Maybe String
 , time : Time.Posix
 , round : Maybe String
 , restreamerOrLocation : String
 , matchType : MatchType
 }

type MatchType = Elo | Friendly | Das

matchTypeToString : MatchType -> String
matchTypeToString m =
  case m of
    Elo -> 
      "Elo"
    Das ->
      "DAS"
    Friendly ->
      "Friendly"






matchParser : Parser TwoPlayerMatch
matchParser =
  Parser.succeed TwoPlayerMatch
    |. Parser.int -- throw away match number; not invariant
    |. Parser.symbol ","
    |= name -- player1Name
    |. Parser.symbol ","
    |= Parser.int -- player1Score
    |. Parser.symbol ","
    |= Parser.int -- player2Score
    |. Parser.symbol ","
    |= name -- player2Name
    |. Parser.symbol ","
    |. Parser.symbol ","
    |= name -- event
    |. Parser.symbol ","
    |= maybeName -- edition
    |. Parser.symbol ","
    |= time -- time
    |. Parser.symbol ","
    |= maybeName -- round
    |. Parser.symbol ","
    |= name -- restreamerOrLocation
    |. Parser.symbol ","
    |= matchType -- matchType

isValidCharForName : Char -> Bool
isValidCharForName char =
  Char.isAlphaNum char
    || char == ' '
    || char == '_'
    || char == '.' -- used in competition names, probably shouldn't be allowed for player names
    || char == '-'

name : Parser String
name =
  Parser.getChompedString <|
    Parser.succeed ()
      |. Parser.chompIf isValidCharForName
      |. Parser.chompWhile isValidCharForName

maybeName : Parser (Maybe String)
maybeName =
  Parser.oneOf
    [ Parser.map Just name
    , Parser.succeed Nothing
    ]
    
cell : Parser String
cell = Parser.getChompedString <| Parser.chompWhile ((/=) ',')

checkTime : String -> Parser Time.Posix
checkTime isoTime =
  case (Iso8601.toTime isoTime) of
    Ok posixTime ->
      Parser.succeed posixTime
    _ ->
      Parser.problem "Time could not be decoded"

time : Parser Time.Posix
time = cell |> Parser.andThen checkTime

checkMatchType : String -> Parser MatchType
checkMatchType mt =
  if 
    mt == "ELO"
  then
    Parser.succeed Elo
  else if
    mt == "DAS"
  then
    Parser.succeed Das
  else if
    mt == "FRIENDLY"
  then
    Parser.succeed Friendly
  else
    Parser.problem "Invalid match type; must be ELO, DAS, or FRIENDLY"

matchType : Parser MatchType
matchType = cell |> Parser.andThen checkMatchType
    
    
    

sampleData : Result String (List TwoPlayerMatch)
sampleData =
  let
    parsedMatches
      = rawSampleData
          |> String.lines
          |> List.map (Parser.run matchParser)
  in
    case firstError parsedMatches of
      Just err ->
        Err err
      _ ->
        Ok (List.filterMap Result.toMaybe parsedMatches)

firstError : List (Result (List Parser.DeadEnd) a) -> Maybe String
firstError = List.filterMap getError >> List.head

getError : Result (List Parser.DeadEnd) a -> Maybe String
getError result =
  case result of
    Err deadEnds ->
      Just (deadEndsToString deadEnds)
    Ok _ ->
      Nothing
  
deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
  "Match parsing error"
    ++ String.join "\n" (List.map deadEndToString deadEnds)

deadEndToString : Parser.DeadEnd -> String
deadEndToString deadEnd =
  let
    textError =
      case deadEnd.problem of
        Parser.Problem p ->
          p
        Parser.ExpectingInt ->
          "Expecting int"
        Parser.ExpectingSymbol symbol ->
          "Expecting symbol: " ++ symbol
        Parser.UnexpectedChar ->
          "Unexpected char"
        Parser.ExpectingEnd ->
          "Expecting end"
        _ ->
          "other error"
  in
  "  at (" 
    ++ String.fromInt deadEnd.row 
    ++ "," 
    ++ String.fromInt deadEnd.col
    ++ "): "
    ++ textError
  


rawSampleData : String
rawSampleData = """55269,DOODLE,1,3,TRAPZONE,,12 Day,December 2025,2025-12-13T20:20:00.000Z,Semis,Pumpyheart,ELO,0,1
55270,PIXELANDY,3,1,WALLBANT,,12 Day,December 2025,2025-12-13T20:20:00.000Z,Semis,Pumpyheart,ELO,1,0
55271,LUKIDELUXE,2,3,RAVIOLI,,TNP,,2025-12-13T21:00:00.000Z,Silver 80,Ravioli,ELO,0,1
55272,RAVIOLI,2,3,LUKIDELUXE,,TNP,,2025-12-13T21:10:00.000Z,Silver 80,Ravioli,ELO,0,1
55273,PIXELANDY,3,0,TRAPZONE,,12 Day,December 2025,2025-12-13T21:45:00.000Z,Finals,Pumpyheart,ELO,1,0
55274,NINJA,3,2,BRIANTHEONE,,CTM Hopefuls,December 2025,2025-12-13T22:36:00.000Z,Round 1,Rxs,ELO,1,0
55275,PEEKAYRIC,4,0,THEDENGLER,,CT League,Season 31,2025-12-14T00:54:00.000Z,Playoffs,Gbtoast,ELO,1,0
55276,HECATE,3,0,THISINTHAT,,TNP Ladder League,SERIES 47,2025-12-14T01:26:00.000Z,639,Moodeuce,ELO,1,0
55277,ALEX T,3,2,VANCE,,DAS League,Season 4,2025-12-14T02:50:00.000Z,1A,Hana,DAS,1,0
55278,BENDY1,3,0,BLUE SCUTI,,DAS League,Season 4,2025-12-14T03:45:00.000Z,1A,Hana,DAS,1,0
55279,DOBRYCZLEK,2,1,SWIDR,,DUBAI L.A.N,2025 Controller,2025-12-14T07:00:00.000Z,First Collection Room 216,Play-In,FRIENDLY,1,0
55280,WYLIE,2,0,SERGIO,,DUBAI L.A.N,2025 Keyboard,2025-12-14T07:00:00.000Z,First Collection Room 214,Quarterfinals,FRIENDLY,1,0
55281,DARK_SHADOW,2,1,ASRVOJTA,,DUBAI L.A.N,2025 Keyboard,2025-12-14T07:15:00.000Z,First Collection Room 214,Quarterfinals,FRIENDLY,1,0
55282,TBRICKS,2,1,COMEGALLETAS,,DUBAI L.A.N,2025 Controller,2025-12-14T07:15:00.000Z,First Collection Room 216,Play-In,FRIENDLY,1,0"""