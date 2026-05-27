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


initialWidth : Int
initialWidth =
    50


initialHeight : Int
initialHeight =
    50


initialTemperature : Float
initialTemperature =
    2.5



-- Exact critical temperature for the 2D square-lattice Ising model when J = 1 and k_B = 1.
-- Below this temperature, the infinite system has non-zero spontaneous magnetization.


criticalTemperature : Float
criticalTemperature =
    2 / logBase e (1 + sqrt 2)


initialUpdatesPerSecond : Int
initialUpdatesPerSecond =
    10000



-- The whole application state. In Elm, the view is a pure function of this Model.


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
    resetModel


initialModel : Model
initialModel =
    { running = False
    , spins = Array.empty
    , width = initialWidth
    , height = initialHeight
    , temperature = initialTemperature
    , updatesPerSecond = initialUpdatesPerSecond
    }


resetModel : ( Model, Cmd Msg )
resetModel =
    ( initialModel
    , Random.generate GotInitialSpins (randomSpins (initialWidth * initialHeight))
    )



-- All events that can change the model. UI events, timer ticks, and random results all return as Msg values.


type Msg
    = ToggleRunning
    | Reset
    | Tick
    | FlipRandomSpin
    | GotInitialSpins (List Spin)
    | GotRandomIndex Int
    | GotAcceptance Int Float
    | GotProposals (List Proposal)
    | SetTemperature String
    | SetCriticalTemperature
    | SetUpdatesPerSecond String


type alias Proposal =
    { index : Int
    , r : Float
    }



-- A classical Ising spin. We use two colors in the UI, but physically these are +1 and -1.


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



-- Random initial states represent a high-entropy, disordered configuration.


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



-- A proposal chooses one spin to try flipping and one random number for Metropolis acceptance.


proposalGenerator : Int -> Random.Generator Proposal
proposalGenerator spinCount =
    Random.map2 Proposal
        (Random.int 0 (spinCount - 1))
        (Random.float 0 1)



-- update is the state transition function. It is pure: side effects are returned as Cmd Msg.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )

        Reset ->
            resetModel

        Tick ->
            if model.running then
                ( model
                  -- Generate many local update proposals per frame, approximating one or more Monte Carlo sweeps.
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

                -- Metropolis rule: always accept lower-energy moves; sometimes accept higher-energy moves.
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

        SetCriticalTemperature ->
            ( { model | temperature = criticalTemperature }, Cmd.none )

        SetUpdatesPerSecond raw ->
            case String.toInt raw of
                Just updatesPerSecond ->
                    ( { model | updatesPerSecond = updatesPerSecond }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


updatesPerTick : Model -> Int
updatesPerTick model =
    max 1 (model.updatesPerSecond // 100)



-- Average spin value in [-1, 1]. Non-zero magnetization is the visible order parameter.


magnetization : Model -> Float
magnetization model =
    let
        spinCount =
            Array.length model.spins
    in
    if spinCount == 0 then
        0

    else
        toFloat (Array.foldl (\spin total -> spinValue spin + total) 0 model.spins) / toFloat spinCount


formatFloat : Int -> Float -> String
formatFloat decimals value =
    let
        scale =
            10 ^ decimals

        rounded =
            round (value * toFloat scale)
    in
    String.fromFloat (toFloat rounded / toFloat scale)


view : Model -> Html Msg
view model =
    div pageStyle
        [ div shellStyle
            [ div headerStyle
                [ div []
                    [ h1 titleStyle [ text "Symmetro" ]
                    , div subtitleStyle [ text "An interactive Ising model for spontaneous symmetry breaking." ]
                    ]
                , div statusBadgeStyle
                    [ text
                        (if model.running then
                            "Running"

                         else
                            "Stopped"
                        )
                    ]
                ]
            , div layoutStyle
                [ div controlPanelStyle
                    [ div metricCardStyle
                        [ div labelStyle [ text "Magnetization" ]
                        , div valueStyle [ text (formatFloat 3 (magnetization model)) ]
                        , div hintStyle [ text "Order parameter in [-1, 1]" ]
                        ]
                    , div buttonRowStyle
                        [ button
                            (primaryButtonStyle ++ [ onClick ToggleRunning ])
                            [ text
                                (if model.running then
                                    "Stop"

                                 else
                                    "Start"
                                )
                            ]
                        , button
                            (secondaryButtonStyle ++ [ onClick Reset ])
                            [ text "Reset" ]
                        ]
                    , button
                        (secondaryButtonStyle ++ [ onClick FlipRandomSpin, style "width" "100%" ])
                        [ text "Flip random spin" ]
                    , div sectionStyle
                        [ div labelStyle [ text ("Temperature: " ++ formatFloat 3 model.temperature) ]
                        , div hintStyle [ text ("Critical Tc = 2 / ln(1 + √2) ≈ " ++ formatFloat 3 criticalTemperature) ]
                        , input
                            (sliderStyle
                                ++ [ Attr.type_ "range"
                                   , Attr.min "0.1"
                                   , Attr.max "5.0"
                                   , Attr.step "0.1"
                                   , Attr.value (String.fromFloat model.temperature)
                                   , onInput SetTemperature
                                   ]
                            )
                            []
                        , button
                            (secondaryButtonStyle ++ [ onClick SetCriticalTemperature, style "margin-top" "10px" ])
                            [ text "Set to critical Tc" ]
                        ]
                    , div sectionStyle
                        [ div labelStyle [ text ("Speed: " ++ String.fromInt model.updatesPerSecond ++ " updates/sec") ]
                        , input
                            (sliderStyle
                                ++ [ Attr.type_ "range"
                                   , Attr.min "100"
                                   , Attr.max "50000"
                                   , Attr.step "100"
                                   , Attr.value (String.fromInt model.updatesPerSecond)
                                   , onInput SetUpdatesPerSecond
                                   ]
                            )
                            []
                        , div hintStyle [ text ("Actual per tick: " ++ String.fromInt (updatesPerTick model)) ]
                        ]
                    ]
                , div visualizationPanelStyle
                    [ viewSpinGrid model ]
                ]
            ]
        ]


pageStyle : List (Html.Attribute Msg)
pageStyle =
    [ style "min-height" "100vh"
    , style "font-family" "Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif"
    , style "background" "radial-gradient(circle at top left, #1f2a44 0, #111827 34%, #030712 100%)"
    , style "color" "#e5e7eb"
    , style "padding" "32px"
    , style "box-sizing" "border-box"
    ]


shellStyle : List (Html.Attribute Msg)
shellStyle =
    [ style "max-width" "1100px"
    , style "margin" "0 auto"
    ]


headerStyle : List (Html.Attribute Msg)
headerStyle =
    [ style "display" "flex"
    , style "justify-content" "space-between"
    , style "align-items" "flex-start"
    , style "gap" "24px"
    , style "margin-bottom" "24px"
    ]


titleStyle : List (Html.Attribute Msg)
titleStyle =
    [ style "margin" "0"
    , style "font-size" "48px"
    , style "letter-spacing" "-0.05em"
    , style "line-height" "1"
    ]


subtitleStyle : List (Html.Attribute Msg)
subtitleStyle =
    [ style "margin-top" "10px"
    , style "color" "#9ca3af"
    , style "font-size" "16px"
    ]


statusBadgeStyle : List (Html.Attribute Msg)
statusBadgeStyle =
    [ style "padding" "8px 12px"
    , style "border" "1px solid rgba(148, 163, 184, 0.3)"
    , style "border-radius" "999px"
    , style "background" "rgba(15, 23, 42, 0.72)"
    , style "font-size" "14px"
    , style "color" "#cbd5e1"
    ]


layoutStyle : List (Html.Attribute Msg)
layoutStyle =
    [ style "display" "grid"
    , style "grid-template-columns" "320px 1fr"
    , style "gap" "24px"
    , style "align-items" "start"
    ]


controlPanelStyle : List (Html.Attribute Msg)
controlPanelStyle =
    [ style "padding" "20px"
    , style "border" "1px solid rgba(148, 163, 184, 0.22)"
    , style "border-radius" "20px"
    , style "background" "rgba(15, 23, 42, 0.72)"
    , style "box-shadow" "0 24px 80px rgba(0, 0, 0, 0.35)"
    , style "backdrop-filter" "blur(14px)"
    ]


visualizationPanelStyle : List (Html.Attribute Msg)
visualizationPanelStyle =
    [ style "padding" "20px"
    , style "border" "1px solid rgba(148, 163, 184, 0.22)"
    , style "border-radius" "20px"
    , style "background" "rgba(3, 7, 18, 0.58)"
    , style "box-shadow" "0 24px 80px rgba(0, 0, 0, 0.35)"
    , style "overflow" "auto"
    ]


metricCardStyle : List (Html.Attribute Msg)
metricCardStyle =
    [ style "padding" "16px"
    , style "border-radius" "16px"
    , style "background" "linear-gradient(135deg, rgba(59, 130, 246, 0.18), rgba(217, 72, 72, 0.18))"
    , style "border" "1px solid rgba(148, 163, 184, 0.2)"
    , style "margin-bottom" "16px"
    ]


labelStyle : List (Html.Attribute Msg)
labelStyle =
    [ style "font-size" "13px"
    , style "font-weight" "700"
    , style "letter-spacing" "0.08em"
    , style "text-transform" "uppercase"
    , style "color" "#cbd5e1"
    ]


valueStyle : List (Html.Attribute Msg)
valueStyle =
    [ style "margin-top" "6px"
    , style "font-size" "34px"
    , style "font-weight" "800"
    , style "letter-spacing" "-0.04em"
    ]


hintStyle : List (Html.Attribute Msg)
hintStyle =
    [ style "margin-top" "6px"
    , style "font-size" "13px"
    , style "line-height" "1.45"
    , style "color" "#94a3b8"
    ]


sectionStyle : List (Html.Attribute Msg)
sectionStyle =
    [ style "margin-top" "20px"
    ]


buttonRowStyle : List (Html.Attribute Msg)
buttonRowStyle =
    [ style "display" "grid"
    , style "grid-template-columns" "1fr 1fr"
    , style "gap" "10px"
    , style "margin-bottom" "10px"
    ]


primaryButtonStyle : List (Html.Attribute Msg)
primaryButtonStyle =
    buttonBaseStyle
        ++ [ style "background" "#e5e7eb"
           , style "color" "#020617"
           ]


secondaryButtonStyle : List (Html.Attribute Msg)
secondaryButtonStyle =
    buttonBaseStyle
        ++ [ style "background" "rgba(15, 23, 42, 0.9)"
           , style "color" "#e5e7eb"
           , style "border" "1px solid rgba(148, 163, 184, 0.3)"
           ]


buttonBaseStyle : List (Html.Attribute Msg)
buttonBaseStyle =
    [ style "border" "0"
    , style "border-radius" "12px"
    , style "padding" "10px 12px"
    , style "font-weight" "700"
    , style "cursor" "pointer"
    ]


sliderStyle : List (Html.Attribute Msg)
sliderStyle =
    [ style "width" "100%"
    , style "margin-top" "10px"
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



-- Apply proposals sequentially. Each accepted flip is visible to later proposals, so this is asynchronous MCMC, not cellular-automaton-style synchronous update.


applyProposals : Model -> List Proposal -> Array Spin
applyProposals model proposals =
    List.foldl (applyProposal model) model.spins proposals


applyProposal : Model -> Proposal -> Array Spin -> Array Spin
applyProposal model proposal spins =
    let
        dE =
            deltaEnergyForSpins model.width model.height proposal.index spins

        -- With h = 0, the rules are symmetric between Up and Down; the final choice is spontaneous.
        accept =
            dE <= 0 || proposal.r < e ^ (-dE / model.temperature)
    in
    if accept then
        flipSpinAt proposal.index spins

    else
        spins



-- Subscriptions turn external time into Elm messages. When stopped, there are no timer events.


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every 10 (\_ -> Tick)

    else
        Sub.none



-- Render the one-dimensional spin array as a two-dimensional square lattice.


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
        , Svg.Attributes.style "display: block;"
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
        -- Soft red.
        Up ->
            "#d94848"

        -- Clear blue.
        Down ->
            "#3b82f6"



-- Sum the four nearest neighbors with periodic boundary conditions, so the grid behaves like a torus.


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



-- Energy change for flipping one spin in the ferromagnetic Ising model with J = 1 and h = 0.
-- ΔE = 2 s_i Σ_neighbors s_j


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
