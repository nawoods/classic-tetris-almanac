module Main exposing (..)
import Browser
import Browser.Navigation as Nav
import Url
import Html

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
  Html.div [] [ Html.text "hi there i'm the classic tetris almanac" ]