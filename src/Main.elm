module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Model =
    { running : Bool }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { running = False }, Cmd.none )


type Msg
    = ToggleRunning


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "system-ui, sans-serif"
        , style "padding" "32px"
        ]
        [ h1 [] [ text "Symmetro" ]
        , div []
            [ text
                (if model.running then
                    "Running"

                 else
                    "Stopped"
                )
            ]
        , button
            [ onClick ToggleRunning
            , style "margin-top" "16px"
            ]
            [ text
                (if model.running then
                    "Stop"

                 else
                    "Start"
                )
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
