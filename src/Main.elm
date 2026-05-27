module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (onClick, onInput)
import Random
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, viewBox, width, x, y)
import Time


type alias Model =
    { running : Bool
    , spins : Array Spin
    , width : Int
    , height : Int
    , temperature : Float
    , updatesPerSecond : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialWidth =
            50

        initialHeight =
            50
    in
    ( { running = False
      , spins = Array.empty
      , width = initialWidth
      , height = initialHeight
      , temperature = 2.5
      , updatesPerSecond = 10000
      }
    , Random.generate GotInitialSpins (randomSpins (initialWidth * initialHeight))
    )


type Msg
    = ToggleRunning
    | Tick
    | FlipRandomSpin
    | GotInitialSpins (List Spin)
    | GotRandomIndex Int
    | GotAcceptance Int Float
    | GotProposals (List Proposal)
    | SetTemperature String
    | SetUpdatesPerSecond String


type alias Proposal =
    { index : Int
    , r : Float
    }


type Spin
    = Up
    | Down


spinValue : Spin -> Int
spinValue spin =
    case spin of
        Up ->
            1

        Down ->
            -1


randomSpin : Random.Generator Spin
randomSpin =
    Random.map
        (\value ->
            if value == 1 then
                Up

            else
                Down
        )
        (Random.int 0 1)


randomSpins : Int -> Random.Generator (List Spin)
randomSpins count =
    Random.list count randomSpin


proposalGenerator : Int -> Random.Generator Proposal
proposalGenerator spinCount =
    Random.map2 Proposal
        (Random.int 0 (spinCount - 1))
        (Random.float 0 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )

        Tick ->
            if model.running then
                ( model
                , Random.generate GotProposals (Random.list (updatesPerTick model) (proposalGenerator (Array.length model.spins)))
                )

            else
                ( model, Cmd.none )

        FlipRandomSpin ->
            ( model
            , Random.generate GotRandomIndex (Random.int 0 (Array.length model.spins - 1))
            )

        GotInitialSpins spins ->
            ( { model | spins = Array.fromList spins }, Cmd.none )

        GotRandomIndex index ->
            ( model
            , Random.generate (GotAcceptance index) (Random.float 0 1)
            )

        GotAcceptance index r ->
            let
                dE =
                    deltaEnergy index model

                accept =
                    dE <= 0 || r < e ^ (-dE / model.temperature)
            in
            if accept then
                ( { model | spins = flipSpinAt index model.spins }, Cmd.none )

            else
                ( model, Cmd.none )

        GotProposals proposals ->
            ( { model | spins = applyProposals model proposals }, Cmd.none )

        SetTemperature raw ->
            case String.toFloat raw of
                Just temperature ->
                    ( { model | temperature = temperature }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetUpdatesPerSecond raw ->
            case String.toInt raw of
                Just updatesPerSecond ->
                    ( { model | updatesPerSecond = updatesPerSecond }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


updatesPerTick : Model -> Int
updatesPerTick model =
    max 1 (model.updatesPerSecond // 100)


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
        , div
            [ style "margin-top" "24px" ]
            [ text ("Temperature: " ++ String.fromFloat model.temperature) ]
        , input
            [ Attr.type_ "range"
            , Attr.min "0.1"
            , Attr.max "5.0"
            , Attr.step "0.1"
            , Attr.value (String.fromFloat model.temperature)
            , onInput SetTemperature
            ]
            []
        , div
            [ style "margin-top" "20px" ]
            [ text ("Speed: " ++ String.fromInt model.updatesPerSecond ++ " updates/sec") ]
        , input
            [ Attr.type_ "range"
            , Attr.min "100"
            , Attr.max "20000"
            , Attr.step "100"
            , Attr.value (String.fromInt model.updatesPerSecond)
            , onInput SetUpdatesPerSecond
            ]
            []
        , div
            [ style "margin-top" "8px" ]
            [ text ("Actual per tick: " ++ String.fromInt (updatesPerTick model)) ]
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


applyProposals : Model -> List Proposal -> Array Spin
applyProposals model proposals =
    List.foldl (applyProposal model) model.spins proposals


applyProposal : Model -> Proposal -> Array Spin -> Array Spin
applyProposal model proposal spins =
    let
        dE =
            deltaEnergyForSpins model.width model.height proposal.index spins

        accept =
            dE <= 0 || proposal.r < e ^ (-dE / model.temperature)
    in
    if accept then
        flipSpinAt proposal.index spins

    else
        spins


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every 10 (\_ -> Tick)

    else
        Sub.none


viewSpinGrid : Model -> Html Msg
viewSpinGrid model =
    let
        cellSize =
            12

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


neighborSumForSpins : Int -> Int -> Int -> Array Spin -> Int
neighborSumForSpins gridWidth gridHeight index spins =
    let
        col =
            modBy gridWidth index

        row =
            index // gridWidth

        idx r c =
            modBy gridHeight r * gridWidth + modBy gridWidth c

        getSpinValue i =
            case Array.get i spins of
                Just spin ->
                    spinValue spin

                Nothing ->
                    0
    in
    getSpinValue (idx (row - 1) col)
        + getSpinValue (idx (row + 1) col)
        + getSpinValue (idx row (col - 1))
        + getSpinValue (idx row (col + 1))


deltaEnergy : Int -> Model -> Float
deltaEnergy index model =
    deltaEnergyForSpins model.width model.height index model.spins


deltaEnergyForSpins : Int -> Int -> Int -> Array Spin -> Float
deltaEnergyForSpins gridWidth gridHeight index spins =
    case Array.get index spins of
        Just spin ->
            2 * toFloat (spinValue spin * neighborSumForSpins gridWidth gridHeight index spins)

        Nothing ->
            0


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
