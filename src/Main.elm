module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, h2, img, input, text)
import Html.Attributes exposing (src, type_, value)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    { input1 : Int
    , input2 : Int
    , input3 : Int
    , sum : Int
    }


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { input1 = 0
            , input2 = 50
            , input3 = 100
            , sum = 0
            }
    in
    ( updateSum initialModel
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SliderChanged SliderInput String


type SliderInput
    = Slider1
    | Slider2
    | Slider3


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SliderChanged slider sliderValue ->
            ( updateSlider slider sliderValue model, Cmd.none )


sliderValueToInt : String -> Int
sliderValueToInt value =
    Maybe.withDefault 0 <| String.toInt value


updateSum : Model -> Model
updateSum model =
    { model | sum = model.input1 + model.input2 + model.input3 }


updateSlider : SliderInput -> String -> Model -> Model
updateSlider sliderName value model =
    let
        updatedModel =
            case sliderName of
                Slider1 ->
                    { model | input1 = sliderValueToInt value }

                Slider2 ->
                    { model | input2 = sliderValueToInt value }

                Slider3 ->
                    { model | input3 = sliderValueToInt value }
    in
    updateSum updatedModel



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text <| String.fromInt model.sum ]
        , sliderInput model.input1 <| SliderChanged Slider1
        , sliderInput model.input2 <| SliderChanged Slider2
        , sliderInput model.input3 <| SliderChanged Slider3
        ]


sliderInput : Int -> (String -> Msg) -> Html Msg
sliderInput value_ tagger =
    let
        sliderValue =
            String.fromInt value_
    in
    div []
        [ text sliderValue
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
