module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, viewBox, width, x, y)
import Time


type alias Model =
    { running : Bool
    , spins : Array Spin
    , width : Int
    , height : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { running = False
      , spins = initialSpins
      , width = 4
      , height = 4
      }
    , Cmd.none
    )


type Msg
    = ToggleRunning
    | Tick
    | FlipRandomSpin
    | GotRandomIndex Int


type Spin
    = Up
    | Down


initialSpins : Array Spin
initialSpins =
    Array.fromList
        [ Up
        , Down
        , Up
        , Down
        , Down
        , Up
        , Down
        , Up
        , Up
        , Up
        , Down
        , Down
        , Down
        , Down
        , Up
        , Up
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )

        Tick ->
            if model.running then
                ( model
                , Random.generate GotRandomIndex (Random.int 0 (Array.length model.spins - 1))
                )

            else
                ( model, Cmd.none )

        FlipRandomSpin ->
            ( model
            , Random.generate GotRandomIndex (Random.int 0 (Array.length model.spins - 1))
            )

        GotRandomIndex index ->
            ( { model | spins = flipSpinAt index model.spins }, Cmd.none )


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
        , button
            [ onClick FlipRandomSpin
            , style "margin-top" "16px"
            , style "margin-left" "8px"
            ]
            [ text "Flip random spin" ]
        , viewSpinGrid model
        ]


flipSpinAt : Int -> Array Spin -> Array Spin
flipSpinAt index spins =
    case Array.get index spins of
        Just spin ->
            Array.set index (flipSpin spin) spins

        Nothing ->
            spins


flipSpin : Spin -> Spin
flipSpin spin =
    case spin of
        Up ->
            Down

        Down ->
            Up


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every 200 (\_ -> Tick)

    else
        Sub.none


viewSpinGrid : Model -> Html Msg
viewSpinGrid model =
    let
        cellSize =
            28

        gridWidth =
            model.width * cellSize

        gridHeight =
            model.height * cellSize
    in
    svg
        [ width (String.fromInt gridWidth)
        , height (String.fromInt gridHeight)
        , viewBox ("0 0 " ++ String.fromInt gridWidth ++ " " ++ String.fromInt gridHeight)
        , Svg.Attributes.style "display: block; margin-top: 24px;"
        ]
        (Array.toList (Array.indexedMap (viewSpinCell model.width cellSize) model.spins))


viewSpinCell : Int -> Int -> Int -> Spin -> Svg Msg
viewSpinCell gridWidth cellSize index spin =
    let
        column =
            modBy gridWidth index

        row =
            index // gridWidth
    in
    rect
        [ x (String.fromInt (column * cellSize))
        , y (String.fromInt (row * cellSize))
        , width (String.fromInt cellSize)
        , height (String.fromInt cellSize)
        , fill (spinColor spin)
        , stroke "#ffffff"
        ]
        []


spinColor : Spin -> String
spinColor spin =
    case spin of
        Up ->
            "#d94848"

        Down ->
            "#3b82f6"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
