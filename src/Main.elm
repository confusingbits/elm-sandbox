module Main exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import GameOfLife exposing (Msg)
import Html exposing (Html, a, b, div, h1, img, li, text, ul)
import Html.Attributes exposing (href, src)
import Html.Events exposing (onClick, onInput)
import List
import Time
import Url exposing (Url)



---- MODEL ----


type alias Model =
    { url : Url, key : Nav.Key, route : Route, currentModel : Maybe GameOfLife.Model }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { url = url, key = key, route = Home, currentModel = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | LinkClicked UrlRequest
    | UrlChanged Url
    | NextGeneration Time.Posix
    | ChangePage Route
    | GotGameOfLifeMsg GameOfLife.Msg


type alias GameOfLifeMsg =
    GameOfLife.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, route = GameOfLife }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    case model.route of
        GameOfLife ->
            let
                ( m, c ) =
                    GameOfLife.init ()

                body =
                    GameOfLife.view m
                        |> Html.map GotGameOfLifeMsg
            in
            Document "game of life" [ body ]

        Home ->
            { title = "hello"
            , body =
                [ div []
                    [ img [ src "/logo.svg" ] []
                    , h1 [] [ text "Your Elm App is working!" ]
                    , div [] [ text "The current URL is: " ]
                    , b [] [ text (Url.toString model.url) ]
                    , ul []
                        [ viewLink "/"
                        , viewLink "/gameoflife"
                        ]
                    ]
                ]
            }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- PAGES ----


type Route
    = Home
    | GameOfLife
