module Main exposing (..)
import Browser
import Browser.Navigation as Nav
import Url
import Html
import Time

import Element as E
import Element.Font as Font

import TwoPlayerMatch

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }





-----------
-- model --
-----------

type Model
  = Matches (List TwoPlayerMatch.TwoPlayerMatch)
  | Error String

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
  case TwoPlayerMatch.sampleData of
    Ok matches ->
      ( Matches matches, Cmd.none )
    Err errorMessage ->
      ( Error errorMessage, Cmd.none )





------------
-- update --
------------

type Msg 
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked _ ->
      ( model, Cmd.none )
    UrlChanged _ ->
      ( model, Cmd.none )





--------------------
-- subsceriptions --
--------------------

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none





----------
-- view --
----------

view : Model -> Browser.Document Msg
view model =
  { title = "The Classic Tetris Almanac"
  , body = [ body model ]
  }

body : Model -> Html.Html Msg
body model =
  E.layout
    []
    <|  case model of
          Matches matches ->
            sampleDataTable matches
          Error error ->
            E.text error

sampleDataTable : List TwoPlayerMatch.TwoPlayerMatch -> E.Element msg
sampleDataTable matches =
  E.column
    [ E.spacing 5
    , E.width E.fill
    ]
    ( List.map sampleDataRow matches )

sampleDataRow : TwoPlayerMatch.TwoPlayerMatch -> E.Element msg
sampleDataRow match =
  E.row []
    [ sampleDataCell 100 match.player1Name
    , sampleDataCell 30 ("(" ++ String.fromInt match.player1Score ++ "-" ++ String.fromInt match.player2Score ++ ")")
    , sampleDataCell 100 match.player2Name
    , sampleDataCell 150 match.event
    , sampleDataCell 150 (Maybe.withDefault "" match.edition)
    , sampleDataCell 150 (Maybe.withDefault "" match.round)
    , sampleDataCell 100 (formattedDate match.time)
    , sampleDataCell 150 match.restreamerOrLocation
    , sampleDataCell 30 (TwoPlayerMatch.matchTypeToString match.matchType)
    ]

sampleDataCell : Int -> String -> E.Element msg
sampleDataCell width content =
  E.el
    [ E.width (E.px width) 
    , Font.size 12
    ]
    ( E.text content )
      
to3LetterMonth : Time.Month -> String 
to3LetterMonth month =
  case month of
    Time.Jan -> "Jan"
    Time.Feb -> "Feb"
    Time.Mar -> "Mar"
    Time.Apr -> "Apr"
    Time.May -> "May"
    Time.Jun -> "Jun"
    Time.Jul -> "Jul"
    Time.Aug -> "Aug"
    Time.Sep -> "Sep"
    Time.Oct -> "Oct"
    Time.Nov -> "Nov"
    Time.Dec -> "Dec"

formattedDate : Time.Posix -> String
formattedDate posix =
  (posix |> Time.toMonth Time.utc |> to3LetterMonth)
    ++ " "
    ++ (posix |> Time.toDay Time.utc |> String.fromInt)
    ++ ", "
    ++ (posix |> Time.toYear Time.utc |> String.fromInt)
