module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, h2, img, input, text)
import Html.Attributes exposing (src, type_, value)
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
    div []
        [ h2 [] [ text <| String.fromFloat model.daysToRunOut ]
        , sliderInput "Rolls" model.rolls <| SliderChanged Rolls
        , sliderInput "People per household" model.people <| SliderChanged People
        , sliderInput "Sheets per roll" model.sheetsPerRoll <| SliderChanged SheetsPerRoll
        ]


sliderInput : String -> Float -> (String -> Msg) -> Html Msg
sliderInput label value_ tagger =
    let
        sliderValue =
            String.fromFloat value_
    in
    div []
        [ text <| sliderValue ++ " "
        , text label
        , input [ onInput tagger, type_ "range", value sliderValue ] []
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
