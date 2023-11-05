module Main exposing (..)

import Browser
import Color
import DecimalNumber as Decimal
import DigitalNumber
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (on)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import List.Extra as ListExtra
import NumberSpinner
import TypedSvg exposing (circle, svg, text_)
import TypedSvg.Attributes exposing (cx, cy, fill, fontFamily, fontSize, r, stroke, strokeWidth, viewBox, x, y)
import TypedSvg.Core as SvgCore exposing (Svg, attribute)
import TypedSvg.Types exposing (Paint(..), px)


type alias Model =
    { spinners : List NumberSpinner.Model
    }


main =
    Browser.sandbox
        { init =
            { spinners =
                [ -- Sign and decimal places
                  NumberSpinner.init
                    2
                    (Decimal.fromInt -100)
                    (Decimal.fromInt 12345)
                    (Decimal.fromIntegralAndDecimals 123 [ 4, 5 ])

                -- No sign, decimal places
                , NumberSpinner.init
                    2
                    (Decimal.fromInt 45)
                    (Decimal.fromInt 12345)
                    (Decimal.fromIntegralAndDecimals 123 [ 4, 5 ])

                -- Sign, no decimal places
                , NumberSpinner.init
                    0
                    (Decimal.fromInt -100)
                    (Decimal.fromInt 12345)
                    (Decimal.fromInt 123)

                -- No sign, no decimal places
                , NumberSpinner.init
                    0
                    (Decimal.fromInt 0)
                    (Decimal.fromInt 12345)
                    (Decimal.fromInt 123)
                ]
            }
        , update = update
        , view = view
        }


type Msg
    = NumberSpinnerMsg Int NumberSpinner.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        NumberSpinnerMsg idx subMsg ->
            case ListExtra.getAt idx model.spinners of
                Nothing ->
                    model

                Just spinner ->
                    let
                        newModel =
                            NumberSpinner.update subMsg spinner
                    in
                    { model | spinners = ListExtra.setAt idx newModel model.spinners }


view : Model -> Html Msg
view model =
    div [] (List.indexedMap (\i spinner -> Html.map (NumberSpinnerMsg i) <| NumberSpinner.view spinner) model.spinners)
