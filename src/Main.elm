module Main exposing (..)

import Browser
import Color
import DigitalNumber
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (on)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import TypedSvg exposing (circle, svg, text_)
import TypedSvg.Attributes exposing (cx, cy, fill, fontFamily, fontSize, r, stroke, strokeWidth, viewBox, x, y)
import TypedSvg.Core as SvgCore exposing (Svg, attribute)
import TypedSvg.Types exposing (Paint(..), px)



-- MAIN


main =
    Browser.sandbox { init = init -10000 12345 -123, update = update, view = view }



-- MODEL


type alias Model =
    { number : DigitalNumber.DigitalNumber
    , currentDigitIndex : Int
    , hasFocus : Bool
    }


init : Int -> Int -> Int -> Model
init minValue maxValue currentValue =
    { number = DigitalNumber.make minValue maxValue currentValue
    , currentDigitIndex = 1
    , hasFocus = False
    }


type Msg
    = HandleKeyboardEvent KeyboardEvent


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleKeyboardEvent { keyCode } ->
            case keyCode of
                Key.Up ->
                    { model
                        | number =
                            if model.currentDigitIndex == 0 then
                                DigitalNumber.increaseSign model.number

                            else
                                DigitalNumber.increaseDigit model.number (model.currentDigitIndex - 1)
                    }

                Key.Down ->
                    { model
                        | number =
                            if model.currentDigitIndex == 0 then
                                DigitalNumber.decreaseSign model.number

                            else
                                DigitalNumber.decreaseDigit model.number (model.currentDigitIndex - 1)
                    }

                Key.Left ->
                    { model
                        | currentDigitIndex =
                            if model.currentDigitIndex > 0 then
                                model.currentDigitIndex - 1

                            else
                                0
                    }

                Key.Right ->
                    { model
                        | currentDigitIndex =
                            if model.currentDigitIndex < DigitalNumber.numberOfDigits model.number then
                                model.currentDigitIndex + 1

                            else
                                model.currentDigitIndex
                    }

                _ ->
                    model



-- VIEW


svgTabindex : Int -> SvgCore.Attribute msg
svgTabindex =
    attribute "tabindex" << String.fromInt


digitFontSize : Int
digitFontSize =
    30


viewDigit : Bool -> Float -> Float -> Char -> Svg msg
viewDigit selected xV yV d =
    text_
        [ x (px xV)
        , y (px yV)
        , fontSize (px (toFloat digitFontSize))
        , fontFamily [ "sans-serif" ]
        , fill <|
            Paint
                (if selected then
                    Color.red

                 else
                    Color.blue
                )
        ]
        [ SvgCore.text (String.fromChar d) ]


view : Model -> Html Msg
view model =
    let
        digitalNumberChars =
            DigitalNumber.toChars model.number

        yPos =
            30.0

        absValueChars : List (Svg msg)
        absValueChars =
            List.indexedMap
                (\i ->
                    viewDigit
                        (i + 1 == model.currentDigitIndex)
                        -- + 1 because the zero'th position is the sign
                        (toFloat ((i + 1) * digitFontSize))
                        yPos
                )
                digitalNumberChars

        signChar : Svg msg
        signChar =
            viewDigit (model.currentDigitIndex == 0) 0.0 yPos <|
                if DigitalNumber.isNegative model.number then
                    '-'

                else
                    '+'
    in
    div []
        [ text (String.fromInt (DigitalNumber.numberValue model.number))
        , svg
            [ viewBox 0 0 800 600
            , svgTabindex 0
            , on "keydown" <| Json.Decode.map HandleKeyboardEvent decodeKeyboardEvent
            ]
            (signChar :: absValueChars)
        ]
