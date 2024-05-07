module Main exposing (..)

import Browser
import Color
import NumberSpinner.DecimalNumber as Decimal
import NumberSpinner.DigitalNumber
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (on)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import List.Extra as ListExtra
import NumberSpinner.NumberSpinner as NumberSpinner
import TypedSvg exposing (circle, svg, text_)
import TypedSvg.Attributes exposing (cx, cy, fill, fontFamily, fontSize, r, stroke, strokeWidth, viewBox, x, y)
import TypedSvg.Core as SvgCore exposing (Svg, attribute)
import TypedSvg.Types exposing (Paint(..), px)


type alias Model =
    { spinners : List (NumberSpinner.Model Msg)
    }


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { spinners =
                        [ -- Sign and decimal places
                          NumberSpinner.init
                            2
                            (Decimal.fromInt 0)
                            (Decimal.fromInt 10)
                            (Maybe.withDefault (Decimal.fromInt 0) <| Decimal.fromString "1.23")
                            (SpinnerMessage 0)

                        -- No sign, decimal places
                        , NumberSpinner.init
                            2
                            (Decimal.fromInt 45)
                            (Decimal.fromInt 12345)
                            (Maybe.withDefault (Decimal.fromInt 0) <| Decimal.fromString "123.45")
                            (SpinnerMessage 1)

                        -- Sign, no decimal places
                        , NumberSpinner.init
                            0
                            (Decimal.fromInt -100)
                            (Decimal.fromInt 12345)
                            (Decimal.fromInt 123)
                            (SpinnerMessage 2)

                        -- No sign, no decimal places
                        , NumberSpinner.init
                            0
                            (Decimal.fromInt 0)
                            (Decimal.fromInt 12345)
                            (Decimal.fromInt 123)
                            (SpinnerMessage 3)
                        ]
                  }
                , Cmd.none
                )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = SpinnerMessage Int NumberSpinner.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpinnerMessage idx subMsg ->
            case ListExtra.getAt idx model.spinners of
                Nothing ->
                    ( model, Cmd.none )

                Just spinner ->
                    let
                        ( newModel, cmd ) =
                            NumberSpinner.update subMsg spinner
                    in
                    ( { model | spinners = ListExtra.setAt idx newModel model.spinners }, cmd )


view : Model -> Html Msg
view model =
    div [ style "width" "50%" ] (List.indexedMap (\i spinner -> NumberSpinner.view spinner) model.spinners)
