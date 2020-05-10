module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, h2, img, input, text)
import Html.Attributes as A exposing (src, type_, value)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    { rolls : Float
    , people : Float
    , sheetsPerRoll : Float
    , daysToRunOut : Float
    }


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { rolls = 8
            , people = 4
            , sheetsPerRoll = 400
            , daysToRunOut = 0
            }
    in
    ( updateDaysToRunOut initialModel
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SliderChanged SliderInput String


type SliderInput
    = Rolls
    | People
    | SheetsPerRoll


type alias SliderConfig =
    { inputName : SliderInput
    , tagger : String -> Msg
    , rangeMin : Int
    , rangeMax : Int
    , stepSize : Int
    , labelText : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SliderChanged slider sliderValue ->
            ( updateSlider slider sliderValue model, Cmd.none )


sliderValueToFloat : String -> Float
sliderValueToFloat value =
    Maybe.withDefault 0 <| String.toFloat value


updateDaysToRunOut : Model -> Model
updateDaysToRunOut model =
    let
        daysToRunOut =
            (model.rolls / model.people)
                * (model.sheetsPerRoll / 1)
                * (1 / 10)
                * (1 / 2)
                * 1
    in
    { model | daysToRunOut = daysToRunOut }


updateSlider : SliderInput -> String -> Model -> Model
updateSlider sliderName value model =
    let
        updatedModel =
            case sliderName of
                Rolls ->
                    { model | rolls = sliderValueToFloat value }

                People ->
                    { model | people = sliderValueToFloat value }

                SheetsPerRoll ->
                    { model | sheetsPerRoll = sliderValueToFloat value }
    in
    updateDaysToRunOut updatedModel



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        daysToRunOut =
            model.daysToRunOut
                |> round
                |> String.fromInt

        daysToRunOutText =
            daysToRunOut ++ " days until you run out"
    in
    div []
        [ h2 [] [ text daysToRunOutText ]
        , sliderInput model.rolls
            { inputName = Rolls
            , tagger = SliderChanged Rolls
            , rangeMin = 1
            , rangeMax = 100
            , labelText = "Rolls"
            , stepSize = 1
            }
        , sliderInput model.people
            { inputName = People
            , tagger = SliderChanged People
            , rangeMin = 1
            , rangeMax = 8
            , labelText = "People per household"
            , stepSize = 1
            }
        , sliderInput model.sheetsPerRoll
            { inputName = SheetsPerRoll
            , tagger = SliderChanged SheetsPerRoll
            , rangeMin = 400
            , rangeMax = 800
            , labelText = "Sheets per roll"
            , stepSize = 50
            }
        ]


sliderInput : Float -> SliderConfig -> Html Msg
sliderInput value_ { inputName, tagger, rangeMin, rangeMax, labelText, stepSize } =
    let
        sliderValue =
            String.fromFloat value_
    in
    div []
        [ text <| sliderValue ++ " "
        , text labelText
        , input
            [ onInput tagger
            , type_ "range"
            , value sliderValue
            , A.min <| String.fromInt rangeMin
            , A.max <| String.fromInt rangeMax
            , A.step <| String.fromInt stepSize
            ]
            []
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
