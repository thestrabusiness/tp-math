module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, input, text)
import Html.Attributes exposing (src, type_, value)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    { input1 : Int
    , input2 : Int
    , input3 : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { input1 = 0
      , input2 = 50
      , input3 = 100
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Slider1Changed String
    | Slider2Changed String
    | Slider3Changed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Slider1Changed sliderValue ->
            ( { model | input1 = sliderValueToInt sliderValue }, Cmd.none )

        Slider2Changed sliderValue ->
            ( { model | input2 = sliderValueToInt sliderValue }, Cmd.none )

        Slider3Changed sliderValue ->
            ( { model | input3 = sliderValueToInt sliderValue }, Cmd.none )


sliderValueToInt : String -> Int
sliderValueToInt value =
    Maybe.withDefault 0 <| String.toInt value



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ sliderInput model.input1 Slider1Changed
        , sliderInput model.input2 Slider2Changed
        , sliderInput model.input3 Slider3Changed
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
