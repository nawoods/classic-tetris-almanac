module Main exposing (..)
import Browser
import Browser.Navigation as Nav
import Url
import Html
import Time
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

type alias Model = Bool

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
  ( True, Cmd.none )





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
  Html.div [] [
    sampleDataTable TwoPlayerMatch.sampleData
  ]


sampleDataTable : Result String (List TwoPlayerMatch.TwoPlayerMatch) -> Html.Html Msg
sampleDataTable data =
  case data of
    Ok matches ->
      Html.div []
      [
        Html.table [] (List.map sampleDataRow matches)
      ]
    Err errorMessage ->
      Html.div [] [ Html.text errorMessage ]

sampleDataRow : TwoPlayerMatch.TwoPlayerMatch -> Html.Html Msg
sampleDataRow match =
  Html.tr []
  [ Html.td [] [ Html.text match.player1Name ]
  , Html.td [] [ Html.text ("(" ++ String.fromInt match.player1Score ++ "-" ++ String.fromInt match.player2Score ++ ")") ]
  , Html.td [] [ Html.text match.player2Name ]
  , Html.td [] [ Html.text match.event ]
  , Html.td [] [ Html.text (Maybe.withDefault "" match.edition) ]
  , Html.td [] [ Html.text (Maybe.withDefault "" match.round) ]
  , Html.td [] [ Html.text (formattedDate match.time) ]
  , Html.td [] [ Html.text match.restreamerOrLocation ]
  , Html.td [] [ Html.text (TwoPlayerMatch.matchTypeToString match.matchType) ]
  ]
      
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
